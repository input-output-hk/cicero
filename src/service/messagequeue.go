package service

import (
	"context"
	"encoding/json"
	model "github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgconn"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
	"log"
	"time"
)

const StartStreamName = "workflow.*.start"
const InvokeStreamName = "workflow.*.*.invoke"
const CertStreamName = "workflow.*.*.cert"

func LiftbridgeConnect(addr string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{addr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
}

//TODO: change to lowercase when service module refactoring is complete.
func CreateStreams(logger *log.Logger, bridge liftbridge.Client, streamNames []string) error {
	for _, streamName := range streamNames {
		if err := bridge.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication(),
		); err != nil {
			if err != liftbridge.ErrStreamExists {
				time.Sleep(1 * time.Second)
				return errors.WithMessage(err, "Failed to Create NATS Stream")
			}
		} else {
			logger.Printf("Created streams %s", streamName)
		}
	}
	return nil
}

//TODO: change to lowercase when service module refactoring is complete.
func Publish(logger *log.Logger, bridge liftbridge.Client, stream, key string, msg model.WorkflowCerts, opts ...liftbridge.MessageOption) error {
	if err := CreateStreams(logger, bridge, []string{stream}); err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	var enc []byte
	if e, err := json.Marshal(msg); err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	} else {
		enc = e
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := bridge.Publish(ctx, stream,
		enc,
		append(opts,
			liftbridge.Key([]byte(key)),
			liftbridge.PartitionByKey(),
			liftbridge.AckPolicyAll(),
		)...,
	); err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	logger.Printf("Published message to stream %s", stream)

	return nil
}

type DBExec interface {
	Exec(context.Context, string, ...interface{}) (pgconn.CommandTag, error)
}

//TODO: change to lowercase when service module refactoring is complete.
func InsertLiftbridgeMessage(logger *log.Logger, db DBExec, msg liftbridge.Message) error {
	headers := msg.Headers()
	delete(headers, "subject")
	for k, v := range headers {
		if v == nil || len(v) == 0 {
			delete(headers, k)
		}
	}

	if _, err := db.Exec(
		context.Background(),
		`INSERT INTO liftbridge_messages ("offset", headers, stream, subject, created_at, value) VALUES ($1, $2, $3, $4, $5, $6)`,
		msg.Offset(), headers, msg.Stream(), msg.Subject(), msg.Timestamp(), msg.Value(),
	); err != nil {
		return errors.WithMessage(err, "Couldn't insert liftbridge message into database")
	}
	return nil
}
