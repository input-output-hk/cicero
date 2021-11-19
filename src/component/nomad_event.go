package component

import (
	"context"
	"log"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
)

type NomadEventConsumer struct {
	Logger                *log.Logger
	MessageQueueService   service.MessageQueueService
	NomadEventService     service.NomadEventService
	WorkflowService       service.WorkflowService
	ActionService         service.ActionService
	WorkflowActionService service.WorkflowActionService
	Db                    *pgxpool.Pool
	NomadClient           *nomad.Client
}

func (self *NomadEventConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting NomadEventConsumer")

	index, err := self.NomadEventService.GetLastNomadEvent()
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}
	index += 1

	self.Logger.Println("Listening to Nomad events starting at index", index)

	stream, err := self.NomadClient.EventStream().Stream(
		ctx,
		map[nomad.Topic][]string{
			nomad.TopicDeployment: {string(nomad.TopicAll)},
			nomad.TopicEvaluation: {string(nomad.TopicAll)},
			nomad.TopicAllocation: {string(nomad.TopicAll)},
			nomad.TopicJob:        {string(nomad.TopicAll)},
			nomad.TopicNode:       {string(nomad.TopicAll)},
		},
		index,
		nil,
	)
	if err != nil {
		return errors.WithMessage(err, "Could not listen to Nomad events")
	}

	for {
		events := <-stream
		if events.Err != nil {
			return errors.WithMessage(err, "Error getting next events from Nomad event stream")
		}

		if events.Index < index {
			// We always get the last event even if we start at
			// an index greater than the last so we have to ignore it.
			// https://github.com/hashicorp/nomad/issues/11296
			continue
		}

		for _, event := range events.Events {
			if err := self.handleNomadEvent(&event); err != nil {
				return errors.WithMessage(err, "Error handling Nomad event")
			}

			//TODO: must be transactional with the message process
			if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
				return self.NomadEventService.Save(tx, &event)
			}); err != nil {
				self.Logger.Println("Could not complete Db transaction")
				return err
			}
		}

		index = events.Index
	}
}

func (self *NomadEventConsumer) handleNomadEvent(event *nomad.Event) error {
	if event.Topic == "Allocation" && event.Type == "AllocationUpdated" {
		allocation, err := event.Allocation()
		if err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's allocation")
		}
		return self.handleNomadAllocationEvent(allocation)
	}
	return nil
}

func (self *NomadEventConsumer) handleNomadAllocationEvent(allocation *nomad.Allocation) error {
	if !allocation.ClientTerminalStatus() {
		self.Logger.Printf("Ignoring allocation event with non-terminal client status \"%s\"", allocation.ClientStatus)
		return nil
	}

	id, err := uuid.Parse(allocation.JobID)
	if err != nil {
		return nil
	}

	action, err := self.ActionService.GetById(id)
	if err != nil {
		if pgxscan.NotFound(err) {
			self.Logger.Printf("Ignoring Nomad event for Job with ID \"%s\" (no such action instance)", allocation.JobID)
			return nil
		}
		return err
	}

	def, err := self.WorkflowActionService.GetWorkflowAction(action)
	if err != nil {
		// TODO We don't want to crash here (for example if a workflow source disappeared)
		// but we don't want to silently ignore it either - should this pop up in the web UI somewhere?
		// Unfortunately evaluators can currently not indicate the reason why evaluation failed - add that to contract?
		self.Logger.Printf("Could not get definition for action instance %s: %s", action.ID, err.Error())
		return nil
	}

	var facts *model.Facts
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

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.ActionService.Update(tx, action); err != nil {
			return errors.WithMessage(err, "Could not update action instance")
		}

		if err := self.MessageQueueService.Publish(
			model.FactStreamName.Fmt(wf.Name, wf.ID),
			model.FactStreamName,
			*facts,
		); err != nil {
			return errors.WithMessage(err, "Could not publish fact")
		}

		return nil
	}); err != nil {
		return errors.WithMessage(err, "Could not complete Db transaction")
	}

	return nil
}