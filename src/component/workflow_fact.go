package component

import (
	"context"
	"encoding/json"
	"log"

	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type WorkflowFactConsumer struct {
	Logger              *log.Logger
	MessageQueueService application.MessageQueueService
	FactService         application.FactService
	ActionService       application.ActionService
	Db                  config.PgxIface
}

func (self *WorkflowFactConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting WorkflowFactConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.FactCreateStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.FactCreateStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

func (self *WorkflowFactConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatalf("Error received in %s: %s", msg.Stream(), err)
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
			return self.processMessage(tx, msg)
		}); err != nil {
			self.Logger.Fatalf("Could not process message: %v with error %s", msg, err)
			return
		}
	}
}

func (self *WorkflowFactConsumer) processMessage(tx pgx.Tx, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessage(err, "Could not save message")
	}

	fact := domain.Fact{}
	if err := json.Unmarshal(msg.Value(), &fact); err != nil {
		return errors.WithMessage(err, "Invalid JSON received")
	}

	// FIXME race condition? TX ends after MQ ACK is received
	// but invoker may have already checked actions against facts by that time,
	// so this fact may not have been visible to the invoker when it looked.
	// ⇒ solution: delete this file, make invoker listen to facts directly.
	if err := self.FactService.Save(tx, &fact); err != nil {
		return err
	}

	if actions, err := self.ActionService.GetCurrent(); err != nil {
		return err
	} else {
		for _, action := range actions {
			if err := self.MessageQueueService.Publish(
				domain.ActionInvokeStream(action.Name),
				domain.ActionInvokeStreamName,
				[]byte{},
			); err != nil {
				self.Logger.Printf("Could not publish invoke message: %s", err)
				return err
			}
		}
	}

	self.Logger.Println("Published invoke messages")

	return nil
}
