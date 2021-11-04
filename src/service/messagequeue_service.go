package service

import (
	"context"
	"encoding/json"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"log"
	"time"
	model "github.com/input-output-hk/cicero/src/model"
)

//TODO: change to lowercase when service module refactoring is complete.
func CreateStreams(logger *log.Logger, bridge liftbridge.Client, streamNames []string) error {
	for _, streamName := range streamNames {
		if err := bridge.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication()); err != nil {
			if err != liftbridge.ErrStreamExists {
				if err != nil {
					time.Sleep(1 * time.Second)
					return errors.WithMessage(err, "Failed to Create NATS Stream")
				}
			}
		} else {
			logger.Printf("Created streams %s\n", streamName)
		}
	}

	return nil
}

//TODO: change to lowercase when service module refactoring is complete.
func Publish(logger *log.Logger, bridge liftbridge.Client, stream, key string, msg model.WorkflowCerts) error {
	err := CreateStreams(logger, bridge, []string{stream})
	if err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	enc, err := json.Marshal(msg)
	if err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	}

	_, err = bridge.Publish(ctx, stream,
		enc,
		liftbridge.Key([]byte(key)),
		liftbridge.PartitionByKey(),
		liftbridge.AckPolicyAll(),
	)

	if err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	logger.Printf("Published message to stream %s\n", stream)

	return nil
}