package component

import (
	"context"
	"github.com/rs/zerolog"
	"time"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
)

type NomadEventConsumer struct {
	Logger              zerolog.Logger
	MessageQueueService application.MessageQueueService
	NomadEventService   application.NomadEventService
	WorkflowService     application.WorkflowService
	ActionService       application.ActionService
	EvaluationService   application.EvaluationService
	Db                  config.PgxIface
	NomadClient         application.NomadClient
}

func (self *NomadEventConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting...")

	index, err := self.NomadEventService.GetLastNomadEvent()
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}
	index += 1

	self.Logger.Debug().Msgf("Listening to Nomad events starting at index %d", index)

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
				self.Logger.Info().Msgf("Processing Nomad Event with index: %d", event.Index)
				return self.processNomadEvent(&event, tx)
			}); err != nil {
				return errors.WithMessagef(err, "Error processing Nomad event with index: %d", event.Index)
			}
		}

		index = events.Index
	}
}
func (self *NomadEventConsumer) processNomadEvent(event *nomad.Event, tx pgx.Tx) error {
	if err := self.handleNomadEvent(event, tx); err != nil {
		return errors.WithMessage(err, "Error handling Nomad event")
	}
	if err := self.NomadEventService.Save(tx, event); err != nil {
		return errors.WithMessage(err, "Error to save Nomad event")
	}
	return nil
}

func (self *NomadEventConsumer) handleNomadEvent(event *nomad.Event, tx pgx.Tx) error {
	if event.Topic == "Allocation" && event.Type == "AllocationUpdated" {
		allocation, err := event.Allocation()
		if err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's allocation")
		}
		return self.handleNomadAllocationEvent(allocation, tx)
	}
	return nil
}

func (self *NomadEventConsumer) getWorkflowAction(action domain.ActionInstance) (*domain.WorkflowAction, error) {
	wf, err := self.WorkflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		return nil, errors.WithMessagef(err, "Could not get workflow instance for workflow instance with ID %d", action.WorkflowInstanceId)
	}

	wfDef, err := self.EvaluationService.EvaluateWorkflow(wf.Source, wf.Name, wf.ID, wf.Facts)
	if err != nil {
		return nil, errors.WithMessagef(err, "Could not evaluate definition for workflow instance %d", wf.ID)
	}

	return wfDef.Actions[action.Name], nil
}

func (self *NomadEventConsumer) handleNomadAllocationEvent(allocation *nomad.Allocation, tx pgx.Tx) error {
	if !allocation.ClientTerminalStatus() {
		self.Logger.Debug().Msgf("Ignoring allocation event with non-terminal client status \"%s\"", allocation.ClientStatus)
		return nil
	}

	id, err := uuid.Parse(allocation.JobID)
	if err != nil {
		return nil
	}

	action, err := self.ActionService.GetById(id)
	if err != nil {
		if pgxscan.NotFound(err) {
			self.Logger.Debug().Msgf("Ignoring Nomad event for Job with ID \"%s\" (no such action instance)", allocation.JobID)
			return nil
		}
		return err
	}

	def, err := self.getWorkflowAction(action)
	if err != nil {
		// TODO We don't want to crash here (for example if a workflow source disappeared)
		// but we don't want to silently ignore it either - should this pop up in the web UI somewhere?
		// Unfortunately evaluators can currently not indicate the reason why evaluation failed - add that to contract?
		self.Logger.Warn().Msgf("Could not get definition for action instance %s: cause: %s", action.ID, err.Error())
		return nil
	}

	var facts *domain.Facts
	switch allocation.ClientStatus {
	case "complete":
		facts = &def.Success
	case "failed":
		facts = &def.Failure
	}
	if facts == nil {
		return nil
	}

	modifyTime := time.Unix(
		allocation.ModifyTime/int64(time.Second),
		allocation.ModifyTime%int64(time.Second),
	).UTC()
	action.FinishedAt = &modifyTime

	wf, err := self.WorkflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		return errors.WithMessagef(err, "Could not get workflow instance for action %s", action.ID)
	}

	if err := self.ActionService.Update(tx, action); err != nil {
		return errors.WithMessage(err, "Could not update action instance")
	}

	if err := self.MessageQueueService.Publish(
		domain.FactStreamName.Fmt(wf.Name, wf.ID),
		domain.FactStreamName,
		*facts,
	); err != nil {
		return errors.WithMessage(err, "Could not publish facts")
	}

	return nil
}
