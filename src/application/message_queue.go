package application

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/rs/zerolog"
	"time"

	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type MessageQueueService interface {
	Publish(string, domain.StreamName, domain.Facts, ...liftbridge.MessageOption) error
	Subscribe(context.Context, domain.StreamName, liftbridge.Handler, int32) error
	Save(pgx.Tx, *liftbridge.Message) error
	BuildMessage(string, []byte) liftbridge.MessageOption
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

func (m *messageQueueService) BuildMessage(name string, value []byte) liftbridge.MessageOption {
	return liftbridge.Header(name, value)
}

func (m *messageQueueService) createStreams(stream []string) error {
	for _, streamName := range stream {
		if err := m.bridge.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication(),
		); err != nil {
			if err != liftbridge.ErrStreamExists {
				time.Sleep(1 * time.Second)
				return errors.WithMessage(err, "Failed to Create NATS Stream")
			}
		} else {
			m.logger.Debug().Msgf("Created streams %s", streamName)
		}
	}
	return nil
}

func (m *messageQueueService) Publish(stream string, streamName domain.StreamName, facts domain.Facts, opts ...liftbridge.MessageOption) error {
	if err := m.createStreams([]string{stream}); err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	var enc []byte
	if e, err := json.Marshal(facts); err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	} else {
		enc = e
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := m.bridge.Publish(ctx, stream,
		enc,
		append(opts,
			liftbridge.Key([]byte(streamName)),
			liftbridge.PartitionByKey(),
			liftbridge.AckPolicyAll(),
		)...,
	); err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	m.logger.Debug().Msgf("Published message to stream %s", stream)

	return nil
}

func (m *messageQueueService) subscribe(ctx context.Context, streamName string, handler liftbridge.Handler, offset int64, partition int32) error {
	m.logger.Debug().Msgf("Subscribing to %s at offset %d", streamName, offset)
	if err := m.bridge.Subscribe(ctx, streamName, handler, liftbridge.StartAtOffset(offset), liftbridge.Partition(partition)); err != nil {
		return errors.WithMessagef(err, "Couldn't Subscribing to %s at offset %d", streamName, offset)
	}
	return nil
}

func (m *messageQueueService) Subscribe(ctx context.Context, streamName domain.StreamName, handler liftbridge.Handler, partition int32) (err error) {
	sName := streamName.String()
	if err := m.createStreams([]string{sName}); err != nil {
		return err
	}
	offset, err := m.getOffset(sName)
	if err != nil {
		return err
	}
	if err := m.subscribe(ctx, sName, handler, offset, partition); err != nil {
		return err
	}
	return
}

func (m *messageQueueService) Save(tx pgx.Tx, message *liftbridge.Message) error {
	m.logger.Info().Msgf("Saving new Message on stream %s, partition %d, offset %d", message.Stream(), message.Partition(), message.Offset())
	headers := message.Headers()
	delete(headers, "subject")
	for k, v := range headers {
		if len(v) == 0 {
			delete(headers, k)
		}
	}
	if err := m.messageQueueRepository.Save(tx, headers, message); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Message")
	}
	m.logger.Debug().Msg("Message created")
	return nil
}

func (m *messageQueueService) getOffset(streamName string) (offset int64, err error) {
	offset, err = m.messageQueueRepository.GetOffset(streamName)
	if err != nil {
		return offset, errors.WithMessagef(err, "Couldn't get the offset for the streamName: %s", streamName)
	}
	m.logger.Info().Msgf("Get Offset %d for the streamName %s", offset, streamName)
	return
}
