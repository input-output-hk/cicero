package application

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"log"
	"os"
	"time"

	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type MessageQueueService interface {
	Publish(string, domain.StreamName, domain.Facts, ...liftbridge.MessageOption) error
	Subscribe(context.Context, domain.StreamName, liftbridge.Handler, int32) error
	Save(pgx.Tx, *liftbridge.Message) error
}

type messageQueueService struct {
	logger                 *log.Logger
	bridge                 liftbridge.Client
	messageQueueRepository repository.MessageQueueRepository
}

func NewMessageQueueService(db *pgxpool.Pool, bridge liftbridge.Client) MessageQueueService {
	return &messageQueueService{
		logger:                 log.New(os.Stderr, "MessageQueueService: ", log.LstdFlags),
		bridge:                 bridge,
		messageQueueRepository: persistence.NewMessageQueueRepository(db),
	}
}

//TODO: move to init method
func LiftbridgeConnect(addr string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{addr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
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
			m.logger.Printf("Created streams %s", streamName)
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

	m.logger.Printf("Published message to stream %s", stream)

	return nil
}

func (m *messageQueueService) subscribe(ctx context.Context, streamName string, handler liftbridge.Handler, offset int64, partition int32) error {
	m.logger.Printf("Subscribing to %s at offset %d", streamName, offset)
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
	m.logger.Printf("Saving new Message %#v", message)
	headers := message.Headers()
	delete(headers, "subject")
	for k, v := range headers {
		if v == nil || len(v) == 0 {
			delete(headers, k)
		}
	}
	if err := m.messageQueueRepository.Save(tx, headers, message); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Message")
	}
	m.logger.Printf("Message created %#v", message)
	return nil
}

func (m *messageQueueService) getOffset(streamName string) (offset int64, err error) {
	offset, err = m.messageQueueRepository.GetOffset(streamName)
	if err != nil {
		return offset, errors.WithMessagef(err, "Couldn't get the offset for the streamName: %s", streamName)
	}
	m.logger.Printf("Get Offset %d for the streamName %s", offset, streamName)
	return
}
