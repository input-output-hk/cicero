package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
)

type BrainCmd struct {
	logger *log.Logger
}

type Workflow struct {
	ID        uint64
	Name      string                 `bun:",notnull"`
	Certs     map[string]interface{} `bun:",notnull"`
	CreatedAt time.Time              `bun:",nullzero,notnull,default:current_timestamp"`
	UpdatedAt time.Time              `bun:",nullzero,notnull,default:current_timestamp"`
}

func (cmd *BrainCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "brain: ", log.LstdFlags)
	}
}

func (cmd *BrainCmd) run() error {
	err := cmd.start(context.Background())
	if err != nil {
		return errors.WithMessage(err, "While running brain")
	}

	for {
		time.Sleep(60 * time.Second)
	}
}

func (cmd *BrainCmd) start(ctx context.Context) error {
	cmd.init()

	if err := cmd.listenToCerts(ctx); err != nil {
		cmd.logger.Printf("Failed to listenToCerts %s\n", err.Error())
		return err
	}

	if err := cmd.listenToStart(ctx); err != nil {
		cmd.logger.Printf("Failed to listenToStart %s\n", err.Error())
		return err
	}

	<-ctx.Done()
	cmd.logger.Println("context was cancelled")
	return nil
}

func (cmd *BrainCmd) listenToStart(ctx context.Context) error {
	cmd.logger.Println("Starting Brain.listenToStart")
	streamName := "workflow.*.start"

	client, err := connect(cmd.logger, []string{streamName})
	if err != nil {
		return err
	}
	defer client.Close()

	cmd.logger.Printf("Subscribing to %s\n", streamName)
	err = client.Subscribe(
		ctx,
		streamName,
		func(msg *liftbridge.Message, err error) {
			if err != nil {
				cmd.logger.Printf("error received in %s: %s", streamName, err.Error())
			}

			cmd.logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), string(msg.Value()))
			cmd.logger.Println("subject:", msg.Subject())
			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]

			cmd.logger.Printf("Received start for workflow %s", workflowName)

			received := map[string]interface{}{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			err = db.RunInTx(context.Background(), nil, func(ctx context.Context, tx bun.Tx) error {
				return cmd.insertWorkflow(tx, &Workflow{Name: workflowName, Certs: received})
			})

			if err != nil {
				cmd.logger.Printf("Failed to insert new workflow: %s\n", err)
			}
		})

	return err
}

func (cmd *BrainCmd) insertWorkflow(db bun.IDB, workflow *Workflow) error {
	res, err := db.
		NewInsert().
		Model(workflow).
		Exec(context.Background())

	if err != nil {
		cmd.logger.Printf("%#v %#v\n", res, err)
		cmd.logger.Printf("Couldn't insert workflow: %s\n", err.Error())
		return err
	}

	cmd.logger.Printf("Created workflow %#v\n", workflow)

	publish(
		cmd.logger,
		fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID),
		"workflow.*.*.invoke",
		workflow.Certs,
	)

	return nil
}

func (cmd *BrainCmd) listenToCerts(ctx context.Context) error {
	cmd.logger.Println("Starting Brain.listenToCerts")
	streamName := "workflow.*.*.cert"

	client, err := connect(cmd.logger, []string{streamName})
	if err != nil {
		return err
	}
	defer client.Close()

	cmd.logger.Printf("Subscribing to %s\n", streamName)
	err = client.Subscribe(
		ctx,
		streamName,
		func(msg *liftbridge.Message, err error) {
			if err != nil {
				cmd.logger.Printf("error received in %s: %s", streamName, err.Error())
			}

			cmd.logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), string(msg.Value()))
			cmd.logger.Println("subject:", msg.Subject())
			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]
			id, err := strconv.ParseUint(parts[2], 10, 64)
			if err != nil {
				cmd.logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			cmd.logger.Printf("Received update for workflow %s %d", workflowName, id)

			received := map[string]interface{}{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			err = db.RunInTx(ctx, nil, func(ctx context.Context, tx bun.Tx) error {
				existing := &Workflow{Name: workflowName}
				err = tx.NewSelect().
					Model(existing).
					Where("id = ?", id).
					Scan(context.Background())

				if err != nil {
					cmd.logger.Printf("Couldn't select existing workflow for id %d: %s\n", id, err)
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
					cmd.logger.Printf("Error while updating workflow: %s", err)
					return err
				}

				// TODO: only invoke when there was a change to the certs?

				cmd.logger.Printf("Updated workflow %#v\n", existing)

				publish(
					cmd.logger,
					fmt.Sprintf("workflow.%s.%d.invoke", workflowName, id),
					"workflow.*.*.invoke",
					merged,
				)

				return err
			})

			if err != nil {
				cmd.logger.Printf("Couldn't complete transaction: %s\n", err)
				return
			}
		}, liftbridge.StartAtEarliestReceived(), liftbridge.Partition(0))

	return errors.WithMessagef(err, "Couldn't subscribe to stream %s", streamName)
}
