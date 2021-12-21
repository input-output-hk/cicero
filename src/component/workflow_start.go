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
			self.Logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
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

		name, source, err := getActionInfo(msg)
		if err != nil {
			self.Logger.Println(err.Error())
			return
		}

		self.Logger.Printf("Received start message for Action with name %q in source %q", name, source)

		var actionDef domain.ActionDefinition
		if def, err := self.EvaluationService.EvaluateAction(source, name, uuid.UUID{}, map[string][]*domain.Fact{}); err != nil {
			self.Logger.Println(err.Error())
			return
		} else {
			actionDef = def
		}

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			if err := self.MessageQueueService.Save(tx, msg); err != nil {
				return errors.WithMessagef(err, "Could not save message %s", msg.Value())
			}

			action := domain.Action{
				Name:   name,
				Source: source,
				Meta:   actionDef.Meta,
				Inputs: actionDef.Inputs,
			}
			if err := self.ActionService.Save(tx, &action); err != nil {
				return err
			}
			self.Logger.Printf("Created Action with ID %q", action.ID)

			return nil
		}); err != nil {
			self.Logger.Fatalf("Could not process message: %s with error %s", msg.Value(), err.Error())
			return
		}

		self.Logger.Printf("Sending Run message for Action with name %q", name)
		if err := self.MessageQueueService.Publish(
			domain.ActionInvokeStream(name),
			domain.ActionInvokeStreamName,
			[]byte{},
		); err != nil {
			self.Logger.Println(err.Error())
			return
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
