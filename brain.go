package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
	"github.com/uptrace/bun/dialect/sqlitedialect"
	"github.com/uptrace/bun/driver/sqliteshim"
)

type BrainCmd struct {
}

type Workflow struct {
	ID    uint64
	Certs map[string]interface{}
}

func runBrain(args *BrainCmd) error {
	sqldb, err := sql.Open(sqliteshim.ShimName, "db/database.sqlite3")
	if err != nil {
		return (errors.WithMessage(err, "While opening the DB"))
	}

	db := bun.NewDB(sqldb, sqlitedialect.New())
	defer db.Close()

	err = brain(db)
	if err != nil {
		return errors.WithMessage(err, "While running brain")
	}

	for {
		time.Sleep(60 * time.Second)
	}
}

func brain(db *bun.DB) error {
	streamName := "workflow.*.cert"

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
			id, err := strconv.ParseUint(parts[1], 10, 64)
			if err != nil {
				logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			logger.Printf("Received update for workflow %d", id)

			received := map[string]interface{}{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			err = db.RunInTx(ctx, nil, func(ctx context.Context, tx bun.Tx) error {
				existing := &Workflow{}
				err = tx.NewSelect().
					Model(existing).
					Where("id = ?", id).
					Scan(context.Background())

				if err == sql.ErrNoRows {
					workflow := &Workflow{
						ID:    id,
						Certs: received,
					}

					_, err = tx.
						NewInsert().
						Model(workflow).
						Exec(context.Background())

					if err != nil {
						logger.Printf("Couldn't insert workflow: %s\n", err)
						return err
					}

					logger.Printf("Created workflow %#v\n", workflow)

					return nil
				} else if err != nil {
					logger.Printf("Couldn't find existing workflow: %s\n", err)
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

				publish(fmt.Sprintf("workflow.%d.invoke", id), "workflow.*.invoke", merged)

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
}
