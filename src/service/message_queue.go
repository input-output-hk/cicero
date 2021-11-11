package service

import (
	"context"
	"encoding/json"
	model "github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"log"
	"os"
	"time"
)

const StartStreamName = "workflow.*.start"
const InvokeStreamName = "workflow.*.*.invoke"
const CertStreamName = "workflow.*.*.cert"

type MessageQueueService interface {
	Publish(string, string, model.WorkflowCerts, ...liftbridge.MessageOption) error
	Subscribe(context.Context, string, liftbridge.Handler, int32) error
	Save(pgx.Tx, *liftbridge.Message) error
}

type MessageQueueServiceImpl struct {
	logger           	   *log.Logger
	bridge 				   liftbridge.Client
	messageQueueRepository repository.MessageQueueRepository
}

func NewMessageQueueService(db *pgxpool.Pool, bridge liftbridge.Client) MessageQueueService {
	return &MessageQueueServiceImpl{
		logger: log.New(os.Stderr, "MessageQueueService: ", log.LstdFlags),
		bridge: bridge,
		messageQueueRepository: repository.NewMessageQueueRepository(db),
	}
}

//TODO: move to init method
func LiftbridgeConnect(addr string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{addr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
}

func (m *MessageQueueServiceImpl) createStreams(streamNames []string) error {
	for _, streamName := range streamNames {
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
			m.logger.Printf("Created streams %s\n", streamName)
		}
	}
	return nil
}

func (m *MessageQueueServiceImpl) Publish(streamNames string, key string, certs model.WorkflowCerts, opts ...liftbridge.MessageOption) error {
	if err := m.createStreams([]string{streamNames}); err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	var enc []byte
	if e, err := json.Marshal(certs); err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	} else {
		enc = e
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := m.bridge.Publish(ctx, streamNames,
		enc,
		append(opts,
			liftbridge.Key([]byte(key)),
			liftbridge.PartitionByKey(),
			liftbridge.AckPolicyAll(),
		)...,
	); err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	m.logger.Printf("Published message to stream %s\n", streamNames)

	return nil
}

func (m *MessageQueueServiceImpl) subscribe(ctx context.Context, streamName string, handler liftbridge.Handler, offset int64, partition int32) error {
	m.logger.Printf("Subscribing to %s at offset %d\n", streamName, offset)
	if err := m.bridge.Subscribe(ctx, streamName, handler, liftbridge.StartAtOffset(offset), liftbridge.Partition(partition)); err != nil {
		return errors.WithMessagef(err, "Couldn't Subscribing to %s at offset %d\n", streamName, offset)
	}
	return nil
}

func (m *MessageQueueServiceImpl) Subscribe(ctx context.Context, streamName string, handler liftbridge.Handler, partition int32) (err error) {
	if err := m.createStreams([]string{streamName}); err != nil {
		return
	}
	offset, err := m.getOffset(streamName)
	if err != nil {
		return
	}
	if err := m.subscribe(ctx, streamName, handler, offset, partition); err != nil {
		return
	}
	return
}

func (m *MessageQueueServiceImpl) Save(tx pgx.Tx, message *liftbridge.Message) error {
	m.logger.Printf("Saving new Message %#v", message)
	if err := m.messageQueueRepository.Save(tx, message); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Message")
	}
	m.logger.Printf("Message created %#v", message)
	return nil
}

func (m *MessageQueueServiceImpl) getOffset(streamName string) (offset int64, err error) {
	offset, err = m.messageQueueRepository.GetOffset(streamName)
	if err != nil {
		return offset, errors.WithMessagef(err, "Couldn't get the offset for the streamName: %s", streamName)
	}
	m.logger.Printf("Get Offset %d for the streamName %s", offset, streamName)
	return
}