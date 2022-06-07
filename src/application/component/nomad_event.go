package component

import (
	"context"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type NomadEventConsumer struct {
	Logger            zerolog.Logger
	FactService       service.FactService
	NomadEventService service.NomadEventService
	RunService        service.RunService
	InvocationService service.InvocationService
	Db                config.PgxIface
	NomadClient       application.NomadClient
}

func (self *NomadEventConsumer) WithQuerier(querier config.PgxIface) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:            self.Logger,
		FactService:       self.FactService.WithQuerier(querier),
		NomadEventService: self.NomadEventService.WithQuerier(querier),
		RunService:        self.RunService.WithQuerier(querier),
		InvocationService: self.InvocationService.WithQuerier(querier),
		Db:                querier,
		NomadClient:       self.NomadClient,
	}
}

func (self *NomadEventConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	if events, err := self.NomadEventService.GetByHandled(false); err != nil {
		return err
	} else {
		self.Logger.Debug().Int("num-unhandled", len(events)).Msg("Handling unhandled events")
		for _, event := range events {
			if err := self.processNomadEvent(ctx, event); err != nil {
				return err
			}
		}
	}

	index, err := self.NomadEventService.GetLastNomadEventIndex()
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}
	index += 1

	self.Logger.Debug().Uint64("index", index).Msg("Listening to Nomad events")

	stream, err := self.NomadClient.EventStream(ctx, index)
	if err != nil {
		return errors.WithMessage(err, "Could not listen to Nomad events")
	}

	for events := range stream {
		if events.Err != nil {
			return errors.WithMessage(events.Err, "Error getting next events from Nomad event stream")
		}

		if events.Index < index {
			// We always get the last event even if we start at
			// an index greater than the last so we have to ignore it.
			// https://github.com/hashicorp/nomad/issues/11296
			continue
		}

		var numConsecutiveAlreadyHandled uint8 = 0
		for _, rawEvent := range events.Events {
			if err := self.processNomadEvent(ctx, &domain.NomadEvent{Event: rawEvent}); err != nil {
				if errors.Is(err, errAlreadyHandled) {
					numConsecutiveAlreadyHandled++
					if numConsecutiveAlreadyHandled == 25 {
						self.Logger.Debug().Msgf(
							"Restarting (%d consecutive events were already handled, I might be too slow to catch up)",
							numConsecutiveAlreadyHandled,
						)
						time.Sleep(1 * time.Second)
						return nil
					}
				} else {
					return err
				}
			} else {
				numConsecutiveAlreadyHandled = 0
			}
		}

		index = events.Index
	}

	return errors.New("Nomad event service finished")
}

var errAlreadyHandled = errors.New("Event has already been handled")

func (self *NomadEventConsumer) processNomadEvent(ctx context.Context, event *domain.NomadEvent) error {
	logger := self.Logger.With().
		Bytes("uid", event.Uid[:]).
		Uint64("index", event.Index).
		Str("topic", string(event.Topic)).
		Str("type", event.Type).
		Logger()

	logger.Trace().Msg("Processing nomad event")

	abort := false

	// Save the event if it's not already in the DB.
	switch {
	case event.Uid == [16]byte{}:
		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			txSelf := self.WithQuerier(tx)

			if err := txSelf.NomadEventService.Save(event); err != nil {
				return err
			}

			if !event.Handled {
				// Immediately flag handled to prevent others from processing it.
				event.Handled = true
				if err := txSelf.NomadEventService.Update(event); err != nil {
					return err
				}
			} else {
				// The event was already put in the DB by another process.
				abort = true
			}

			return nil
		}); err != nil {
			return err
		}
	case event.Handled:
		// No need to handle an event that's already in the DB and flagged as handled.
		abort = true
	default:
		event.Handled = true
		if err := self.NomadEventService.Update(event); err != nil {
			return err
		}
	}

	if abort {
		logger.Trace().AnErr("reason", errAlreadyHandled).Msg("Abort processing nomad event")
		return errAlreadyHandled
	}

	if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		if err := txSelf.handleNomadEvent(ctx, &event.Event); err != nil {
			// FIXME if we crash before unflagging the event will never be handled
			// save a timestamp and consider it unflagged after a while?

			logger.Err(err).Msg("Error handling nomad event")

			event.Handled = false
			if err := txSelf.NomadEventService.Update(event); err != nil {
				return err
			}

			return errors.WithMessage(err, "Error handling nomad event")
		}

		return nil
	}); err != nil {
		return errors.WithMessagef(err, "Error processing nomad event with index %d and uid %q", event.Index, event.Uid)
	}

	logger.Trace().Msg("Processed nomad event")

	return nil
}

func (self *NomadEventConsumer) handleNomadEvent(ctx context.Context, event *nomad.Event) error {
	switch event.Topic {
	case "Allocation":
		return self.handleNomadAllocationEvent(ctx, event)
	case "Job":
		return self.handleNomadJobEvent(ctx, event)
	default:
		self.Logger.Trace().
			Str("topic", string(event.Topic)).
			Str("type", string(event.Type)).
			Msg("Ignoring event")
		return nil
	}
}

func (self *NomadEventConsumer) handleNomadAllocationEvent(ctx context.Context, event *nomad.Event) error {
	switch event.Type {
	case "AllocationUpdated":
	default:
		self.Logger.Trace().
			Str("topic", string(event.Topic)).
			Str("type", string(event.Type)).
			Msg("Ignoring event")
		return nil
	}

	allocation, err := event.Allocation()
	if err != nil {
		return errors.WithMessage(err, "Error getting Nomad event's allocation")
	}

	logger := self.Logger.With().
		Str("nomad-job-id", allocation.JobID).
		Logger()

	switch allocation.ClientStatus {
	case nomad.AllocClientStatusFailed, nomad.AllocClientStatusLost:
	default:
		logger.Trace().
			Str("client-status", allocation.ClientStatus).
			Msg("Ignoring allocation event (client status is not failure)")
		return nil
	}

	if allocation.NextAllocation != "" {
		self.Logger.Trace().
			Str("next-allocation", allocation.NextAllocation).
			Msg("Ignoring allocation event (rescheduled)")
		return nil
	}

	run, err := self.getRun(logger, allocation.JobID)
	if run == nil || err != nil {
		return err
	}

	return self.endRun(ctx, run, allocation.ModifyTime, domain.RunStatusFailed)
}

func (self *NomadEventConsumer) handleNomadJobEvent(ctx context.Context, event *nomad.Event) error {
	switch event.Type {
	case "AllocationUpdated", "JobDeregistered":
	default:
		self.Logger.Trace().
			Str("topic", string(event.Topic)).
			Str("type", string(event.Type)).
			Msg("Ignoring event")
		return nil
	}

	job, err := event.Job()
	if err != nil {
		return errors.WithMessage(err, "Error getting Nomad event's job")
	}

	logger := self.Logger.With().
		Str("nomad-job-id", *job.ID).
		Logger()

	switch event.Type {
	case "AllocationUpdated":
		if *job.Status != "dead" {
			logger.Trace().
				Str("nomad-job-id", *job.ID).
				Str("status", *job.Status).
				Msg("Ignoring job event (status not dead)")
			return nil
		}
	case "JobDeregistered":
		if !*job.Stop {
			logger.Trace().
				Str("nomad-job-id", *job.ID).
				Bool("stop", *job.Stop).
				Msg("Ignoring job event (not stopping)")
			return nil
		}
	default:
		panic("should have been caught by switch above")
	}

	run, err := self.getRun(logger, *job.ID)
	if run == nil || err != nil {
		return err
	}

	allocs, _, err := self.NomadClient.JobsAllocations(*job.ID, false, &nomad.QueryOptions{})
	if err != nil {
		return err
	}

	if len(allocs) == 0 {
		panic("Found no allocations for this job")
	}

	modifyTime := allocs[0].ModifyTime
	for _, alloc := range allocs[1:] {
		if alloc.ModifyTime > modifyTime {
			modifyTime = alloc.ModifyTime
		}
	}

	return self.endRun(ctx, run, modifyTime, domain.RunStatusSucceeded)
}

func (self *NomadEventConsumer) getRun(logger zerolog.Logger, idStr string) (*domain.Run, error) {
	id, err := uuid.Parse(idStr)
	if err != nil {
		logger.Trace().Msg("Ignoring event (ID is not a UUID)")
		return nil, nil
	}

	run, err := self.RunService.GetByNomadJobIdWithLock(id, "FOR NO KEY UPDATE")
	if err != nil {
		if pgxscan.NotFound(err) {
			logger.Trace().Msg("Ignoring event (no such Run)")
			return nil, nil
		}
		return &run, err
	}

	if run.FinishedAt != nil {
		logger.Trace().Msg("Ignoring event (Run already finished)")
		return nil, nil
	}

	return &run, nil
}

func (self *NomadEventConsumer) endRun(ctx context.Context, run *domain.Run, timestamp int64, status domain.RunStatus) error {
	return self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		switch run.Status {
		default:
			panic(`endRun() called on Run with status "` + run.Status.String() + `"`)
		case domain.RunStatusCanceled:
		case domain.RunStatusRunning:
			run.Status = status

			if output, err := txSelf.InvocationService.GetOutputById(run.InvocationId); err != nil {
				return err
			} else {
				fact := domain.Fact{
					RunId: &run.NomadJobID,
				}

				switch run.Status {
				case domain.RunStatusSucceeded:
					if output.Success.Exists() {
						fact.Value = output.Success
					}
				case domain.RunStatusFailed:
					if output.Failure.Exists() {
						fact.Value = output.Failure
					}
				default:
					panic("should have been caught in switch above")
				}

				if fact.Value != nil {
					if err := txSelf.FactService.Save(&fact, nil); err != nil {
						return err
					}
				}
			}
		}

		modifyTime := time.Unix(
			timestamp/int64(time.Second),
			timestamp%int64(time.Second),
		).UTC()
		run.FinishedAt = &modifyTime

		return txSelf.RunService.End(run)
	})
}
