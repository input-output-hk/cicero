package component

import (
	"context"
	"strings"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionCreateConsumer struct {
	Logger              zerolog.Logger
	MessageQueueService service.MessageQueueService
	ActionService       service.ActionService
	EvaluationService   service.EvaluationService
	Db                  config.PgxIface
}

func (self *ActionCreateConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	if err := self.MessageQueueService.Subscribe(ctx, domain.ActionCreateStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to stream %s", domain.ActionCreateStreamName)
	}

	<-ctx.Done()
	self.Logger.Debug().Msg("context was cancelled")
	return nil
}

// TODO build generalized template for these consumer/subscriber thingies to reduce boilerplate
func (self *ActionCreateConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
			// TODO: If err is not nil, the subscription will be terminated
			return
		}

		self.Logger.Debug().
			Str("stream", msg.Stream()).
			Str("subject", msg.Subject()).
			Int64("offset", msg.Offset()).
			Str("value:", string(msg.Value())).
			Time("time:", msg.Timestamp()).
			Msg("Received message")

		name, source, err := getActionInfo(msg)
		if err != nil {
			self.Logger.Err(err).Send()
			return
		}

		self.Logger.Printf("Received start message for Action with name %q in source %q", name, source)

		action := domain.Action{
			ID:     uuid.New(),
			Name:   name,
			Source: source,
		}

		var actionDef domain.ActionDefinition
		if def, err := self.EvaluationService.EvaluateAction(source, name, action.ID); err != nil {
			self.Logger.Err(err).Send()
			return
		} else {
			actionDef = def
		}

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			if err := self.MessageQueueService.Save(tx, msg); err != nil {
				return errors.WithMessagef(err, "Could not save message %s", msg.Value())
			}

			action.Meta = actionDef.Meta
			action.Inputs = actionDef.Inputs
			if err := self.ActionService.Save(tx, &action); err != nil {
				return err
			}

			return nil
		}); err != nil {
			self.Logger.Fatal().Err(err).Bytes("message", msg.Value()).Msg("Could not process message")
			return
		}

		self.Logger.Info().Str("name", name).Msg("Sending Run message for Action")
		if err := self.MessageQueueService.Publish(
			domain.ActionInvokeStream(name),
			domain.ActionInvokeStreamName,
			[]byte{},
		); err != nil {
			self.Logger.Err(err).Send()
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
