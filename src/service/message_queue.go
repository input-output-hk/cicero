package service

import (
	"context"
	"encoding/json"
	model "github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgconn"
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
	CreateStreams([]string) error
	Publish(string, string, model.WorkflowCerts, ...liftbridge.MessageOption) error
	//GetOffset(string) (int64, error)
	//Save(pgx.Tx, *liftbridge.Message) error
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

func LiftbridgeConnect(addr string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{addr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
}

func (m *MessageQueueServiceImpl) CreateStreams(streamNames []string) error {
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

//TODO streamNames as string or []string?
func (m *MessageQueueServiceImpl) Publish(streamNames string, key string, certs model.WorkflowCerts, opts ...liftbridge.MessageOption) error {
	if err := m.CreateStreams([]string{streamNames}); err != nil {
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

type DBExec interface {
	Exec(context.Context, string, ...interface{}) (pgconn.CommandTag, error)
}

//TODO: change to lowercase when service module refactoring is complete.
func InsertLiftbridgeMessage(logger *log.Logger, db DBExec, msg *liftbridge.Message) error {
	if _, err := db.Exec(
		context.Background(),
		`INSERT INTO liftbridge_messages ("offset", stream, subject, created_at, value) VALUES ($1, $2, $3, $4, $5)`,
		msg.Offset(), msg.Stream(), msg.Subject(), msg.Timestamp(), msg.Value(),
	); err != nil {
		return errors.WithMessage(err, "Couldn't insert liftbridge message into database")
	}
	return nil
}
