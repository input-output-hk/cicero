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
	Db                config.PgxIface
	NomadClient       application.NomadClient
}

func (self *NomadEventConsumer) WithQuerier(querier config.PgxIface) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:            self.Logger,
		FactService:       self.FactService.WithQuerier(querier),
		NomadEventService: self.NomadEventService.WithQuerier(querier),
		RunService:        self.RunService.WithQuerier(querier),
		Db:                querier,
		NomadClient:       self.NomadClient,
	}
}

func (self *NomadEventConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	index, err := self.NomadEventService.GetLastNomadEvent()
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}
	index += 1

	self.Logger.Debug().Uint64("index", index).Msg("Listening to Nomad events")

	stream, err := self.NomadClient.EventStream(ctx, index)
	if err != nil {
		return errors.WithMessage(err, "Could not listen to Nomad events")
	}

	for {
		events := <-stream
		if events.Err != nil {
			return errors.WithMessage(events.Err, "Error getting next events from Nomad event stream")
		}

		if events.Index < index {
			// We always get the last event even if we start at
			// an index greater than the last so we have to ignore it.
			// https://github.com/hashicorp/nomad/issues/11296
			continue
		}

		for _, event := range events.Events {
			if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
				self.Logger.Trace().Uint64("index", event.Index).Msg("Processing Nomad Event")
				return self.WithQuerier(tx).processNomadEvent(&event)
			}); err != nil {
				return errors.WithMessagef(err, "Error processing Nomad event with index: %d", event.Index)
			}
		}

		index = events.Index
	}
}

func (self *NomadEventConsumer) processNomadEvent(event *nomad.Event) error {
	if err := self.handleNomadEvent(event); err != nil {
		return errors.WithMessage(err, "Error handling Nomad event")
	}
	if err := self.NomadEventService.Save(event); err != nil {
		return errors.WithMessage(err, "Error to save Nomad event")
	}
	return nil
}

func (self *NomadEventConsumer) handleNomadEvent(event *nomad.Event) error {
	switch {
	case event.Topic == "Allocation" && event.Type == "AllocationUpdated":
		if allocation, err := event.Allocation(); err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's allocation")
		} else {
			return self.handleNomadAllocationEvent(allocation)
		}
	case event.Topic == "Job" && event.Type == "AllocationUpdated":
		if job, err := event.Job(); err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's job")
		} else {
			return self.handleNomadJobEvent(job)
		}
	default:
		self.Logger.Trace().
			Str("topic", string(event.Topic)).
			Str("type", string(event.Type)).
			Msg("Ignoring event")
	}
	return nil
}

func (self *NomadEventConsumer) handleNomadAllocationEvent(allocation *nomad.Allocation) error {
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

	id, err := uuid.Parse(allocation.JobID)
	if err != nil {
		return nil
	}

	run, err := self.RunService.GetByNomadJobId(id)
	if err != nil {
		if pgxscan.NotFound(err) {
			logger.Debug().Msg("Ignoring event (no such Run)")
			return nil
		}
		return err
	}

	if output, err := self.RunService.GetOutputByNomadJobId(id); err != nil && !pgxscan.NotFound(err) {
		return err
	} else if output.Failure != nil {
		fact := domain.Fact{
			RunId: &run.NomadJobID,
			Value: output.Failure,
		}
		if err := self.FactService.Save(&fact, nil); err != nil {
			return errors.WithMessage(err, "Could not publish Fact")
		}
	}

	return self.endRunAt(&run, allocation.ModifyTime)
}

func (self *NomadEventConsumer) handleNomadJobEvent(job *nomad.Job) error {
	logger := self.Logger.With().
		Str("nomad-job-id", *job.ID).
		Logger()

	if *job.Status != "dead" {
		logger.Trace().Msg("Ignoring job event (status is not dead)")
		return nil
	}

	id, err := uuid.Parse(*job.ID)
	if err != nil {
		return err
	}

	run, err := self.RunService.GetByNomadJobId(id)
	if err != nil {
		if pgxscan.NotFound(err) {
			logger.Debug().Msg("Ignoring job event (no such Run)")
			return nil
		}
		return err
	}

	if output, err := self.RunService.GetOutputByNomadJobId(id); err != nil && !pgxscan.NotFound(err) {
		return err
	} else if output.Success != nil {
		fact := domain.Fact{
			RunId: &run.NomadJobID,
			Value: output.Success,
		}
		if err := self.FactService.Save(&fact, nil); err != nil {
			return errors.WithMessage(err, "Could not publish Fact")
		}
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

	return self.endRunAt(&run, modifyTime)
}

func (self *NomadEventConsumer) endRunAt(run *domain.Run, timestamp int64) error {
	modifyTime := time.Unix(
		timestamp/int64(time.Second),
		timestamp%int64(time.Second),
	).UTC()
	run.FinishedAt = &modifyTime

	if err := self.RunService.End(run); err != nil {
		return errors.WithMessagef(err, "Failed to end Run with ID %q", run.NomadJobID)
	}

	return nil
}
