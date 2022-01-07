package component

import (
	"bytes"
	"context"
	"encoding/json"
	"io"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type FactCreateConsumer struct {
	Logger              zerolog.Logger
	MessageQueueService service.MessageQueueService
	FactService         service.FactService
	ActionService       service.ActionService
	Db                  config.PgxIface
}

func (self *FactCreateConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting")

	if err := self.MessageQueueService.Subscribe(ctx, domain.FactCreateStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to stream %s", domain.FactCreateStreamName)
	}

	<-ctx.Done()
	self.Logger.Debug().Msg("context was cancelled")
	return nil
}

func (self *FactCreateConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Err(err).Str("stream", msg.Stream()).Msg("Error received")
			//TODO: If err is not nil, the subscription will be terminated
			return
		}

		self.Logger.Debug().
			Str("stream", msg.Stream()).
			Str("subject", msg.Subject()).
			Int64("offset", msg.Offset()).
			Str("value", string(msg.Value())).
			Time("time", msg.Timestamp()).
			Msg("Received message")

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(tx, msg)
		}); err != nil {
			self.Logger.Fatal().Err(err).Bytes("message", msg.Value()).Msg("Could not process message")
			return
		}
	}
}

func (self *FactCreateConsumer) processMessage(tx pgx.Tx, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessage(err, "Could not save message")
	}

	valueReader := bytes.NewReader(msg.Value())

	fact := domain.Fact{}
	factDecoder := json.NewDecoder(valueReader)
	if err := factDecoder.Decode(&fact); err != nil {
		return errors.WithMessage(err, "Invalid JSON received")
	}

	// We save as new Fact if no ID is given.
	// XXX We should always save first and then only notify.
	// Or have two streams: A creates and B notifies. A would call B.
	if fact.ID == (uuid.UUID{}) {
		// FIXME race condition? TX ends after MQ ACK is received
		// but invoker may have already checked actions against facts by that time,
		// so this fact may not have been visible to the invoker when it looked.
		// ⇒ solution: delete this file, make invoker listen to facts directly.
		if err := self.FactService.Save(tx, &fact, io.MultiReader(factDecoder.Buffered(), valueReader)); err != nil {
			return err
		}
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
				self.Logger.Err(err).Msg("Could not publish invoke message")
				return err
			}
		}
	}

	self.Logger.Debug().Msg("Published invoke messages")

	return nil
}
