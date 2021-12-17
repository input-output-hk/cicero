package component

import (
	"context"
	"log"
	"strings"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionStartConsumer struct {
	Logger              *log.Logger
	MessageQueueService application.MessageQueueService
	ActionService       application.ActionService
	EvaluationService   application.EvaluationService
	Db                  config.PgxIface
}

func (self *ActionStartConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting ActionStartConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.ActionCreateStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to stream %s", domain.ActionCreateStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

// TODO build generalized template for these consumer/subscriber thingies to reduce boilerplate
func (self *ActionStartConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Printf("error received in %s: %w", msg.Stream(), err)
			// TODO: If err is not nil, the subscription will be terminated
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

		if name, source, err := getActionInfo(msg); err != nil {
			self.Logger.Println(err.Error())
			return
		} else {
			self.Logger.Printf("Received start message for Action with name %q in source %q", name, source)

			if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
				return self.processMessage(tx, name, source, msg)
			}); err != nil {
				self.Logger.Fatalf("Could not process message: %v with error %w", msg, err)
				return
			}
		}
	}
}

func getActionInfo(msg *liftbridge.Message) (name, source string, err error) {
	name = strings.Split(msg.Subject(), ".")[1]
	// XXX since inputs are not included in the message anymore
	// it may make sense to simply put source (and name?)
	// in the body as json for simpler handling.
	// currently the body is always empty and ignored...
	if sourceFromMsg, sourceGiven := msg.Headers()["source"]; !sourceGiven {
		err = errors.Errorf("No source given for Action %q", name)
		return
	} else {
		source = string(sourceFromMsg)
	}
	return
}

func (self *ActionStartConsumer) processMessage(tx pgx.Tx, actionName, source string, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessagef(err, "Could not save message %v", msg)
	}

	var actionDef domain.ActionDefinition
	// TODO look at the func params ..?
	if def, err := self.EvaluationService.EvaluateAction(actionName, source, uuid.UUID{}, map[string][]*domain.Fact{}); err != nil {
		return err
	} else {
		actionDef = def
	}

	action := domain.Action{
		Name:   actionName,
		Source: source,
		Meta:   actionDef.Meta,
		Inputs: actionDef.Inputs,
	}
	if err := self.ActionService.Save(tx, &action); err != nil {
		return err
	}
	self.Logger.Printf("Created Action with ID %q", action.ID)

	self.Logger.Printf("Sending Run message for Action with ID %q", action.ID)
	if err := self.MessageQueueService.Publish(
		domain.ActionInvokeStream(actionName),
		domain.ActionInvokeStreamName,
		[]byte{},
	); err != nil {
		return err
	}

	return nil
}
