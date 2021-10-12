package main

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
)

type BrainCmd struct {
}

type Workflow struct {
	ID        uint64
	Name      string                 `bun:",notnull"`
	Certs     map[string]interface{} `bun:",notnull"`
	CreatedAt time.Time              `bun:",nullzero,notnull,default:current_timestamp"`
	UpdatedAt time.Time              `bun:",nullzero,notnull,default:current_timestamp"`
}

func runBrain(args *BrainCmd) error {
	db, err := newDb()
	defer db.Close()
	if err != nil { return err }

	err = brain(db)
	if err != nil {
		return errors.WithMessage(err, "While running brain")
	}

	for {
		time.Sleep(60 * time.Second)
	}
}

func brain(db *bun.DB) error {
	if err := listenToCerts(db); err != nil {
		return err
	}

	return listenToStart(db)
}

func listenToStart(db *bun.DB) error {
	streamName := "workflow.*.start"

	client := connect([]string{streamName})
	defer client.Close()

	ctx := context.Background()
	err := client.Subscribe(
		ctx,
		streamName,
		func(msg *liftbridge.Message, err error) {
			if err != nil {
				logger.Printf("error received in %s: %w", streamName, err)
			}

			fmt.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), string(msg.Value()))
			fmt.Println("subject:", msg.Subject())
			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]

			logger.Printf("Received start for workflow %s", workflowName)

			received := map[string]interface{}{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			err = db.RunInTx(context.Background(), nil, func(ctx context.Context, tx bun.Tx) error {
				return insertWorkflow(tx, &Workflow{Name: workflowName, Certs: received})
			})

			if err != nil {
				logger.Printf("Failed to insert new workflow: %s\n", err)
			}
		})

	return err
}

func insertWorkflow(db bun.IDB, workflow *Workflow) error {
	res, err := db.
		NewInsert().
		Model(workflow).
		Exec(context.Background())

	if err != nil {
		fmt.Printf("%#v %#v\n", res, err)
		logger.Printf("Couldn't insert workflow: %s\n", err.Error())
		return err
	}

	logger.Printf("Created workflow %#v\n", workflow)

	publish(fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID), "workflow.*.*.invoke", workflow.Certs)

	return nil
}

func listenToCerts(db *bun.DB) error {
	streamName := "workflow.*.*.cert"

	client := connect([]string{streamName})
	defer client.Close()

	ctx := context.Background()
	err := client.Subscribe(
		ctx,
		streamName,
		func(msg *liftbridge.Message, err error) {
			if err != nil {
				logger.Printf("error received in brain stream: %w", err)
			}

			fmt.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), string(msg.Value()))
			fmt.Println("subject:", msg.Subject())
			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]
			id, err := strconv.ParseUint(parts[2], 10, 64)
			if err != nil {
				logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			logger.Printf("Received update for workflow %s %d", workflowName, id)

			received := map[string]interface{}{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			err = db.RunInTx(ctx, nil, func(ctx context.Context, tx bun.Tx) error {
				existing := &Workflow{Name: workflowName}
				err = tx.NewSelect().
					Model(existing).
					Where("id = ?", id).
					Scan(context.Background())

				if err != nil {
					logger.Printf("Couldn't select existing workflow for id %d: %s\n", id, err)
					return err
				}

				merged := map[string]interface{}{}

				for k, v := range existing.Certs {
					merged[k] = v
				}

				for k, v := range received {
					merged[k] = v
				}

				existing.Certs = merged
				existing.UpdatedAt = time.Now().UTC()

				_, err = tx.NewUpdate().
					Where("id = ?", id).
					Model(existing).
					Exec(context.Background())

				if err != nil {
					logger.Printf("Error while updating workflow: %s", err)
					return err
				}

				// TODO: only invoke when there was a change to the certs?

				logger.Printf("Updated workflow %#v\n", existing)

				publish(fmt.Sprintf("workflow.%s.%d.invoke", workflowName, id), "workflow.*.*.invoke", merged)

				return err
			})

			if err != nil {
				logger.Printf("Couldn't complete transaction: %s\n", err)
				return
			}
		}, liftbridge.StartAtEarliestReceived(), liftbridge.Partition(0))

	return errors.WithMessagef(err, "Couldn't subscribe to stream %s", streamName)
}

func connect(streamNames []string) liftbridge.Client {
	client, err := liftbridge.Connect([]string{"localhost:9292"})
	fail(errors.WithMessage(err, "Couldn't connect to NATS"))

	// TODO remove this
	time.Sleep(3 * time.Second)

	for _, streamName := range streamNames {
		if err := client.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication()); err != nil {
			if err != liftbridge.ErrStreamExists {
				fail(errors.WithMessage(err, "Failed to Create NATS Stream"))
			}
		} else {
			logger.Printf("Created streams %s\n", streamName)
		}
	}

	return client
}

func fail(err error) {
	if err != nil {
		panic(err)
	}
}

func publish(stream string, key string, msg map[string]interface{}) {
	client := connect([]string{stream})
	defer client.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	enc, err := json.Marshal(msg)
	fail(errors.WithMessage(err, "Failed to encode JSON"))

	_, err = client.Publish(ctx, stream,
		enc,
		liftbridge.Key([]byte(key)),
		liftbridge.PartitionByKey(),
		liftbridge.AckPolicyAll(),
	)

	fail(errors.WithMessage(err, "While publishing message"))

	logger.Printf("Published message to stream %s\n", stream)
}
