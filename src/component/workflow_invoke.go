package component

import (
	"context"
	"encoding/json"
	"log"
	"strings"
	"time"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type InvokeConsumer struct {
	Logger              *log.Logger
	Limiter             *priority.PriorityLimiter
	EvaluationService   application.EvaluationService
	ActionService       application.ActionService
	RunService          application.RunService
	MessageQueueService application.MessageQueueService
	Db                  config.PgxIface
	NomadClient         application.NomadClient
}

func (self *InvokeConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting InvokeConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.ActionInvokeStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to stream %s", domain.ActionInvokeStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

func (self *InvokeConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatalf("error in liftbridge message: %s", err.Error())
			//TODO: If err is not nil, the subscription will be terminated
			return
		}

		self.Logger.Println(
			"Received message",
			"stream:", msg.Stream(),
			"subject:", msg.Subject(),
			"offset:", msg.Offset(),
			"value:", string(msg.Value()),
			"time:", msg.Timestamp(),
		)

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(ctx, tx, msg)
		}); err != nil {
			self.Logger.Fatalf("Could not process message: %s with error %s", msg.Value(), err.Error())
			return
		}
	}
}

func (self *InvokeConsumer) getActionName(msg *liftbridge.Message) string {
	return strings.Split(msg.Subject(), ".")[1]
}

func (self *InvokeConsumer) processMessage(ctx context.Context, tx pgx.Tx, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessage(err, "Could not save message")
	}

	if action, err := self.ActionService.GetLatestByName(self.getActionName(msg)); err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			self.Logger.Println("No Action with that name, ignoring invoke message")
		} else {
			return err
		}
	} else if runnable, inputs, err := self.ActionService.IsRunnable(&action); err != nil {
		return err
	} else if runnable {
		self.Limiter.Wait(ctx, priority.High) // TODO: What is the ctx to use in this case?
		defer self.Limiter.Finish()

		var facts []interface{}
		if actionDef, err := self.EvaluationService.EvaluateAction(action.Source, action.Name, action.ID, inputs); err != nil {
			return err
		} else if actionDef.IsDecision() {
			facts = actionDef.Success
		} else {
			run := domain.Run{
				ActionId:  action.ID,
				Success:   actionDef.Success,
				Failure:   actionDef.Failure,
				CreatedAt: time.Now(),
			}

			if err := self.RunService.Save(tx, &run); err != nil {
				return errors.WithMessage(err, "Could not insert Run")
			}

			runId := run.NomadJobID.String()
			actionDef.Job.ID = &runId

			if response, _, err := self.NomadClient.JobsRegister(actionDef.Job, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to run Action")
			} else if len(response.Warnings) > 0 {
				self.Logger.Printf("Warnings occured registering Nomad job %q in Nomad evaluation %q: %s", runId, response.EvalID, response.Warnings)
			}
		}

		var errs error
		for _, fact := range facts {
			if factJson, err := json.Marshal(fact); err != nil {
				errs = errors.WithMessagef(err, "Could not marshal fact: %w", errs)
			} else if err := self.MessageQueueService.Publish(
				domain.FactCreateStreamName.String(),
				domain.FactCreateStreamName,
				factJson,
			); err != nil {
				errs = errors.WithMessagef(err, "Could not publish fact: %w", errs)
			}
		}
		return errors.WithMessage(errs, "Failed to publish all facts")
	}

	return nil
}
