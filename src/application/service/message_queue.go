package service

import (
	"context"
	"time"

	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type MessageQueueService interface {
	Publish(string, domain.StreamName, []byte, ...liftbridge.MessageOption) error
	Subscribe(context.Context, domain.StreamName, liftbridge.Handler, int32) error
	Save(pgx.Tx, *liftbridge.Message) error
}

type messageQueueService struct {
	logger                 zerolog.Logger
	bridge                 liftbridge.Client
	messageQueueRepository repository.MessageQueueRepository
}

func NewMessageQueueService(db config.PgxIface, bridge liftbridge.Client, logger *zerolog.Logger) MessageQueueService {
	return &messageQueueService{
		logger:                 logger.With().Str("component", "MessageQueueService").Logger(),
		bridge:                 bridge,
		messageQueueRepository: persistence.NewMessageQueueRepository(db),
	}
}

func (m *messageQueueService) createStream(stream string) error {
	if err := m.bridge.CreateStream(
		context.Background(),
		stream, stream,
		liftbridge.MaxReplication(),
	); err != nil {
		if err != liftbridge.ErrStreamExists {
			time.Sleep(1 * time.Second)
			return errors.WithMessage(err, "Failed to create NATS stream")
		}
	} else {
		m.logger.Debug().Str("stream", stream).Msg("Created stream")
	}
	return nil
}

func (m *messageQueueService) Publish(stream string, streamName domain.StreamName, message []byte, opts ...liftbridge.MessageOption) error {
	if err := m.createStream(stream); err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := m.bridge.Publish(
		ctx, string(stream), message,
		append(opts,
			liftbridge.Key([]byte(streamName)),
			liftbridge.PartitionByKey(),
			liftbridge.AckPolicyAll(),
		)...,
	); err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	m.logger.Debug().Str("stream", stream).Msg("Published message")

	return nil
}

func (m *messageQueueService) subscribe(ctx context.Context, streamName domain.StreamName, handler liftbridge.Handler, offset int64, partition int32) error {
	m.logger.Debug().Str("stream", streamName.String()).Int64("offset", offset).Msg("Subscribing")
	if err := m.bridge.Subscribe(ctx, streamName.String(), handler, liftbridge.StartAtOffset(offset), liftbridge.Partition(partition)); err != nil {
		return errors.WithMessagef(err, "Could not subscribe to %s at offset %d", streamName, offset)
	}
	return nil
}

func (m *messageQueueService) Subscribe(ctx context.Context, streamName domain.StreamName, handler liftbridge.Handler, partition int32) error {
	if err := m.createStream(streamName.String()); err != nil {
		return err
	}
	if offset, err := m.getOffset(streamName.String()); err != nil {
		return err
	} else if err := m.subscribe(ctx, streamName, handler, offset, partition); err != nil {
		return err
	}
	return nil
}

func (m *messageQueueService) Save(tx pgx.Tx, message *liftbridge.Message) error {
	m.logger.Debug().
		Str("stream", message.Stream()).
		Int32("partition", message.Partition()).
		Int64("offset", message.Offset()).
		Msg("Saving new Message")
	headers := message.Headers()
	delete(headers, "subject")
	for k, v := range headers {
		if len(v) == 0 {
			delete(headers, k)
		}
	}
	if err := m.messageQueueRepository.Save(tx, headers, message); err != nil {
		return errors.WithMessagef(err, "Could not insert Message")
	}
	m.logger.Debug().Msg("Message created")
	return nil
}

func (m *messageQueueService) getOffset(streamName string) (offset int64, err error) {
	m.logger.Debug().Str("stream", streamName).Msg("Getting offset")
	offset, err = m.messageQueueRepository.GetOffset(streamName)
	if err != nil {
		return offset, errors.WithMessagef(err, "Could not get the offset for the streamName: %s", streamName)
	}
	m.logger.Debug().Str("stream", streamName).Int64("offset", offset).Msg("Got offset")
	return
}
