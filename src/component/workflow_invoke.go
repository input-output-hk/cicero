package component

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

type WorkflowInvokeConsumer struct {
	Logger              *log.Logger
	Limiter             *priority.PriorityLimiter
	EvaluationService   application.EvaluationService
	ActionService       application.ActionService
	MessageQueueService application.MessageQueueService
	WorkflowService     application.WorkflowService
	Db                  config.PgxIface
	NomadClient         application.NomadClient
}

func (self *WorkflowInvokeConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting WorkflowInvokeConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.InvokeStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.InvokeStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

func (self *WorkflowInvokeConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatalf("error in liftbridge message: %s", err.Error())
			//TODO: If err is not nil, the subscription will be terminated
		}

		wMessageDetail, err := self.getWorkflowDetails(msg)
		if err != nil {
			self.Logger.Printf("Invalid Workflow ID received, ignoring: %s with error %s", msg.Subject(), err)
			return
		}

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(ctx, tx, wMessageDetail, msg)
		}); err != nil {
			self.Logger.Println(fmt.Printf("Could not process workflow invoke message: %v with error %s", msg, err))
			return
		}
	}
}

func (self *WorkflowInvokeConsumer) getWorkflowDetails(msg *liftbridge.Message) (*domain.WorkflowInstance, error) {
	parts := strings.Split(msg.Subject(), ".")
	if len(parts) < 3 {
		return nil, fmt.Errorf("Invalid Message received, ignoring: %s", msg.Subject())
	}
	workflowName := parts[1]
	wfInstanceId, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		return nil, errors.WithMessagef(err, "Invalid Workflow ID received, ignoring: %s", msg.Subject())
	}
	inputs := domain.Facts{}
	if err := json.Unmarshal(msg.Value(), &inputs); err != nil {
		return nil, errors.Errorf("Invalid JSON received, ignoring: %s", err)
	}
	self.Logger.Printf("Invoked workflow %s %d", workflowName, wfInstanceId)
	self.Logger.Println(
		"stream:", msg.Stream(),
		"subject:", msg.Subject(),
		"offset:", msg.Offset(),
		"value:", string(msg.Value()),
		"time:", msg.Timestamp(),
	)
	return &domain.WorkflowInstance{
		ID:    wfInstanceId,
		Name:  workflowName,
		Facts: inputs,
	}, nil
}

func (self *WorkflowInvokeConsumer) processMessage(ctx context.Context, tx pgx.Tx, workflow *domain.WorkflowInstance, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		self.Logger.Printf("%s", err)
		return err
	}

	wf, err := self.WorkflowService.GetById(workflow.ID)
	if err != nil {
		return errors.WithMessage(err, "Could not find workflow instance with ID %d")
	}

	// We don't actually need workflowName because the instance already knows its name.
	// We would only need it if instance IDs were not globally unique, but only per workflow name.
	// TODO Decide whether we want instance IDs to be unique per workflow name or globally.
	if wf.Name != workflow.Name {
		return errors.New("Workflow name given does not match name of instance: " + workflow.Name + " != " + wf.Name)
	}

	workflowDefinition, err := self.EvaluationService.EvaluateWorkflow(wf.Source, workflow.Name, workflow.ID, workflow.Facts)
	if err != nil {
		return errors.WithMessage(err, "Invalid Workflow Definition, ignoring")
	}

	for actionName, action := range workflowDefinition.Actions {
		if err := self.invokeWorkflowAction(ctx, tx, workflow.ID, workflow.Facts, actionName, action); err != nil {
			return err
		}
	}

	return nil
}

func (self *WorkflowInvokeConsumer) invokeWorkflowAction(ctx context.Context, tx pgx.Tx, wfInstanceId uint64, inputs domain.Facts, actionName string, action *domain.WorkflowAction) error {
	self.Limiter.Wait(ctx, priority.High) //TODO: What is the ctx to use in this case?
	defer self.Limiter.Finish()

	var instance *domain.ActionInstance
	if inst, err := self.ActionService.GetByNameAndWorkflowId(actionName, wfInstanceId); err != nil {
		if !pgxscan.NotFound(err) {
			return errors.WithMessage(err, "While getting last action instance")
		}
	} else {
		instance = &inst
	}

	self.Logger.Printf("Checking runnability of %s: %v", actionName, action.IsRunnable())

	if action.IsRunnable() {
		if instance == nil {
			instance = &domain.ActionInstance{}
			instance.WorkflowInstanceId = wfInstanceId
			instance.Name = actionName
			instance.Facts = inputs

			if err := self.ActionService.Save(tx, instance); err != nil {
				return errors.WithMessage(err, "Could not insert action instance")
			}
		} else {
			updatedAt := time.Now().UTC()
			instance.UpdatedAt = &updatedAt
			instance.Facts = inputs
			if err := self.ActionService.Update(tx, *instance); err != nil {
				return errors.WithMessage(err, "Could not update action instance")
			}
		}

		actionInstanceId := instance.ID.String()
		action.Job.ID = &actionInstanceId

		if response, _, err := self.NomadClient.JobsRegister(&action.Job, &nomad.WriteOptions{}); err != nil {
			return errors.WithMessage(err, "Failed to run action")
		} else if len(response.Warnings) > 0 {
			self.Logger.Println(response.Warnings)
		}

	} else if instance != nil {
		if _, _, err := self.NomadClient.JobsDeregister(instance.ID.String(), false, &nomad.WriteOptions{}); err != nil {
			return errors.WithMessage(err, "Failed to stop action")
		}

		finished := time.Now().UTC()
		instance.FinishedAt = &finished

		if err := self.ActionService.Update(tx, *instance); err != nil {
			return errors.WithMessage(err, "Failed to update action instance")
		}
	}

	return nil
}
