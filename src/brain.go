package cicero

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"cirello.io/oversight"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
)

type BrainCmd struct {
	logger *log.Logger
	tree   *oversight.Tree
	bridge liftbridge.Client
}

func GetDefinition(w model.WorkflowInstance, logger *log.Logger) (model.WorkflowDefinition, error) {
	var def model.WorkflowDefinition

	var certs []byte
	certs, err := json.Marshal(w.Certs)
	if err != nil {
		return def, err
	}

	def, err = nixInstantiateWorkflow(logger, w.Name, w.ID, string(certs))
	if err != nil {
		return def, err
	}

	return def, nil
}

func (cmd *BrainCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "brain: ", log.LstdFlags)
	}

	if cmd.tree == nil {
		cmd.tree = oversight.New(oversight.WithSpecification(
			10,                    // number of restarts
			10*time.Minute,        // within this time period
			oversight.OneForOne(), // restart every task on its own
		))
	}

}

func (cmd *BrainCmd) Run() error {
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

	cmd.tree.Add(cmd.listenToCerts)
	cmd.tree.Add(cmd.listenToStart)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := cmd.tree.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting brain supervisor")
	}

	<-ctx.Done()
	cmd.logger.Println("context was cancelled")
	return nil
}

func (cmd *BrainCmd) listenToStart(ctx context.Context) error {
	cmd.logger.Println("Starting Brain.listenToStart")
	streamName := "workflow.*.start"

	err := service.CreateStreams(cmd.logger, cmd.bridge, []string{streamName})
	if err != nil {
		return err
	}

	cmd.logger.Printf("Subscribing to %s\n", streamName)
	err = cmd.bridge.Subscribe(
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

			err = DB.RunInTx(context.Background(), nil, func(ctx context.Context, tx bun.Tx) error {
				return cmd.insertWorkflow(tx, &model.WorkflowInstance{Name: workflowName, Certs: received})
			})

			if err != nil {
				cmd.logger.Printf("Failed to insert new workflow: %s\n", err)
			}
		})

	if err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", streamName)
	}

	<-ctx.Done()
	cmd.logger.Println("context was cancelled")
	return nil
}

func (cmd *BrainCmd) insertWorkflow(db bun.IDB, workflow *model.WorkflowInstance) error {
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

	service.Publish(
		cmd.logger,
		cmd.bridge,
		fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID),
		"workflow.*.*.invoke",
		workflow.Certs,
	)

	return nil
}

func (cmd *BrainCmd) listenToCerts(ctx context.Context) error {
	cmd.logger.Println("Starting Brain.listenToCerts")
	streamName := "workflow.*.*.cert"

	err := service.CreateStreams(cmd.logger, cmd.bridge, []string{streamName})
	if err != nil {
		return err
	}

	cmd.logger.Printf("Subscribing to %s\n", streamName)
	err = cmd.bridge.Subscribe(
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

			err = DB.RunInTx(ctx, nil, func(ctx context.Context, tx bun.Tx) error {
				existing := &model.WorkflowInstance{Name: workflowName}
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

				service.Publish(
					cmd.logger,
					cmd.bridge,
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

	if err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", streamName)
	}

	<-ctx.Done()
	cmd.logger.Println("context was cancelled")
	return nil
}
