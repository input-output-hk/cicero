package cicero

import (
	"context"
	"encoding/json"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"

	"cirello.io/oversight"
	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

type Invoker struct {
	logger              *log.Logger
	tree                *oversight.Tree
	limiter             *priority.PriorityLimiter
	evaluator           *Evaluator
	actionService       service.ActionService
	messageQueueService service.MessageQueueService
	workflowService     service.WorkflowService
	db                  *pgxpool.Pool
	nomadClient         *nomad.Client
}

func (self *Invoker) listenToInvoke(ctx context.Context) error {
	self.logger.Println("Starting Invoker.listenToInvoke")

	if err := self.messageQueueService.Subscribe(ctx, model.InvokeStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", model.InvokeStreamName)
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Invoker) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.logger.Fatalf("error in liftbridge message: %s", err.Error())
		}

		inputs := model.Facts{}
		if err := json.Unmarshal(msg.Value(), &inputs); err != nil {
			self.logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)
			self.logger.Printf("Invalid JSON received, ignoring: %s", err)
			return
		}

		parts := strings.Split(msg.Subject(), ".")
		workflowName := parts[1]
		wfInstanceId, err := strconv.ParseUint(parts[2], 10, 64)
		if err != nil {
			self.logger.Printf("Invalid Workflow Instance ID received, ignoring: %s", msg.Subject())
			return
		}

		//TODO: must be transactional with the message process
		if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			err := self.messageQueueService.Save(tx, msg)
			return err
		}); err != nil {
			self.logger.Printf("Could not complete db transaction")
			return
		}

		if err := self.invokeWorkflow(ctx, workflowName, wfInstanceId, inputs); err != nil {
			self.logger.Println("Failed to invoke workflow", err)
		}
	}
}

func (self *Invoker) invokeWorkflow(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.Facts) error {
	wf, err := self.workflowService.GetById(wfInstanceId)
	if err != nil {
		return errors.WithMessage(err, "Could not find workflow instance with ID %d")
	}

	// We don't actually need workflowName because the instance already knows its name.
	// We would only need it if instance IDs were not globally unique, but only per workflow name.
	// TODO Decide whether we want instance IDs to be unique per workflow name or globally.
	if wf.Name != workflowName {
		return errors.New("Workflow name given does not match name of instance: " + workflowName + " != " + wf.Name)
	}

	workflow, err := self.evaluator.EvaluateWorkflow(wf.Source, wf.Name, wfInstanceId, inputs)
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

func (self *Invoker) invokeWorkflowAction(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.Facts, actionName string, action *model.WorkflowAction) error {
	self.limiter.Wait(context.Background(), priority.High)
	defer self.limiter.Finish()

	var instance *model.ActionInstance
	if inst, err := self.actionService.GetByNameAndWorkflowId(actionName, wfInstanceId); err != nil {
		if !pgxscan.NotFound(err) {
			return errors.WithMessage(err, "While getting last action instance")
		}
	} else {
		instance = &inst
	}

	self.logger.Printf("Checking runnability of %s: %v", actionName, action.IsRunnable())

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if action.IsRunnable() {
			if instance == nil {
				instance = &model.ActionInstance{}
				instance.WorkflowInstanceId = wfInstanceId
				instance.Name = actionName
				instance.Facts = inputs

				err := self.actionService.Save(tx, instance)
				if err != nil {
					return errors.WithMessage(err, "Could not insert action instance")
				}
			} else {
				updatedAt := time.Now().UTC()
				instance.UpdatedAt = &updatedAt
				instance.Facts = inputs
				if err := self.actionService.Update(tx, *instance); err != nil {
					return errors.WithMessage(err, "Could not update action instance")
				}
			}

			actionInstanceId := instance.ID.String()
			action.Job.ID = &actionInstanceId

			if response, _, err := self.nomadClient.Jobs().Register(&action.Job, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to run action")
			} else if len(response.Warnings) > 0 {
				self.logger.Println(response.Warnings)
			}

		} else if instance != nil {
			if _, _, err := self.nomadClient.Jobs().Deregister(instance.ID.String(), false, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to stop action")
			}

			finished := time.Now().UTC()
			instance.FinishedAt = &finished

			if err := self.actionService.Update(tx, *instance); err != nil {
				return errors.WithMessage(err, "Failed to update action instance")
			}
		}
		return nil
	}); err != nil {
		return err
	}

	return nil
}
