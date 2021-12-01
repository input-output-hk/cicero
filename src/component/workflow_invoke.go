package component

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
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
	Db                  *pgxpool.Pool
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
		}

		inputs := domain.Facts{}
		if err := json.Unmarshal(msg.Value(), &inputs); err != nil {
			self.Logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)
			self.Logger.Printf("Invalid JSON received, ignoring: %s", err)
			return
		}

		parts := strings.Split(msg.Subject(), ".")
		workflowName := parts[1]
		wfInstanceId, err := strconv.ParseUint(parts[2], 10, 64)
		if err != nil {
			self.Logger.Printf("Invalid Workflow Instance ID received, ignoring: %s", msg.Subject())
			return
		}

		//TODO: must be transactional with the message process
		if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			err := self.MessageQueueService.Save(tx, msg)
			return err
		}); err != nil {
			self.Logger.Printf("Could not complete Db transaction")
			return
		}

		if err := self.invokeWorkflow(ctx, workflowName, wfInstanceId, inputs); err != nil {
			self.Logger.Println("Failed to invoke workflow", err)
		}
	}
}

func (self *WorkflowInvokeConsumer) invokeWorkflow(ctx context.Context, workflowName string, wfInstanceId uint64, inputs domain.Facts) error {
	wf, err := self.WorkflowService.GetById(wfInstanceId)
	if err != nil {
		return errors.WithMessage(err, "Could not find workflow instance with ID %d")
	}

	// We don't actually need workflowName because the instance already knows its name.
	// We would only need it if instance IDs were not globally unique, but only per workflow name.
	// TODO Decide whether we want instance IDs to be unique per workflow name or globally.
	if wf.Name != workflowName {
		return errors.New("Workflow name given does not match name of instance: " + workflowName + " != " + wf.Name)
	}

	workflow, err := self.EvaluationService.EvaluateWorkflow(wf.Source, wf.Name, wfInstanceId, inputs)
	if err != nil {
		return errors.WithMessage(err, "Invalid Workflow Definition, ignoring")
	}

	for actionName, action := range workflow.Actions {
		if err := self.invokeWorkflowAction(ctx, wf.Name, wfInstanceId, inputs, actionName, action); err != nil {
			return err
		}
	}

	return nil
}

func (self *WorkflowInvokeConsumer) invokeWorkflowAction(ctx context.Context, workflowName string, wfInstanceId uint64, inputs domain.Facts, actionName string, action *domain.WorkflowAction) error {
	self.Limiter.Wait(context.Background(), priority.High)
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

	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if action.IsRunnable() {
			if instance == nil {
				instance = &domain.ActionInstance{}
				instance.WorkflowInstanceId = wfInstanceId
				instance.Name = actionName
				instance.Facts = inputs

				err := self.ActionService.Save(tx, instance)
				if err != nil {
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
	}); err != nil {
		return err
	}

	return nil
}
