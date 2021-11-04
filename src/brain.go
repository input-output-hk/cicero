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
	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
)

type BrainCmd struct {
	logger    *log.Logger
	tree      *oversight.Tree
	bridge    liftbridge.Client
	workflowService service.WorkflowService
	evaluator Evaluator
}

func (cmd *BrainCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "brain: ", log.LstdFlags)
	}
	if cmd.workflowService == nil {
		wfService := &service.WorkflowServiceCmd{}
		wfService.Init(DB)
		cmd.workflowService = wfService
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

func (cmd *BrainCmd) addToTree(tree *oversight.Tree) {
	tree.Add(cmd.listenToCerts)
	tree.Add(cmd.listenToStart)
	tree.Add(cmd.listenToNomadEvents)
}

func (cmd *BrainCmd) start(ctx context.Context) error {
	cmd.init()

	cmd.addToTree(cmd.tree)

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

			received := model.WorkflowCerts{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			if err = cmd.insertWorkflow(ctx, &model.WorkflowInstance{Name: workflowName, Certs: received}); err != nil {
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

func (cmd *BrainCmd) insertWorkflow(ctx context.Context, workflow *model.WorkflowInstance) error {
	tx, err := DB.Begin(ctx)
	if err != nil {
		cmd.logger.Printf("%s\n", err)
		return err
	}
	err = cmd.workflowService.Save(tx, workflow)

	if err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	cmd.logger.Printf("Created workflow with ID %d\n", workflow.ID)

	service.Publish(
		cmd.logger,
		cmd.bridge,
		fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID),
		"workflow.*.*.invoke",
		workflow.Certs,
	)

	err = tx.Commit(context.Background())

	if err != nil {
		cmd.logger.Printf("Couldn't complete transaction: %s\n", err)
	}

	return err
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

			received := model.WorkflowCerts{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			tx, err := DB.Begin(ctx)
			if err != nil {
				cmd.logger.Printf("%s\n", err)
				return
			}

			wf, err := cmd.workflowService.GetById(id)
			var existing = &wf
			if err != nil {
				return
			}

			merged := model.WorkflowCerts{}

			for k, v := range existing.Certs {
				merged[k] = v
			}

			for k, v := range received {
				merged[k] = v
			}

			now := time.Now().UTC()
			existing.Certs = merged
			existing.UpdatedAt = &now

			_, err = cmd.workflowService.Update(tx, id, existing)

			if err != nil {
				cmd.logger.Printf("Error while updating workflow: %s", err)
				return
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

			err = tx.Commit(context.Background())

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

func (cmd *BrainCmd) listenToNomadEvents(ctx context.Context) error {
	cmd.logger.Println("Starting Brain.listenToNomadEvents")

	var index uint64
	if err := DB.QueryRow(
		context.Background(),
		`SELECT COALESCE(MAX("index") + 1, 0) FROM nomad_events`,
	).Scan(&index); err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}

	cmd.logger.Println("Listening to Nomad events starting at index", index)

	stream, err := nomadClient.EventStream().Stream(
		ctx,
		map[nomad.Topic][]string{
			nomad.TopicAll: {string(nomad.TopicAll)},
		},
		index,
		nil,
	)
	if err != nil {
		return errors.WithMessage(err, "Could not listen to Nomad events")
	}

	for {
		events := <-stream
		if events.Err != nil {
			return errors.WithMessage(err, "Error getting next events from Nomad event stream")
		}

		for _, event := range events.Events {
			if err := cmd.handleNomadEvent(event); err != nil {
				return errors.WithMessage(err, "Error handling Nomad event")
			}

			if _, err := DB.Exec(
				context.Background(),
				`INSERT INTO nomad_events (topic, "type", "key", filter_keys, "index", payload) VALUES ($1, $2, $3, $4, $5, $6)`,
				event.Topic, event.Type, event.Key, event.FilterKeys, event.Index, event.Payload,
			); err != nil {
				return errors.WithMessage(err, "Could not insert Nomad event into database")
			}

			index = event.Index
		}
	}
}

func (cmd *BrainCmd) handleNomadEvent(event nomad.Event) error {
	switch {
	case event.Topic == "Allocation" && event.Type == "AllocationUpdated":
		allocation, err := event.Allocation()
		if err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's allocation")
		}
		return cmd.handleNomadAllocationEvent(allocation)
	}
	return nil
}

func (cmd *BrainCmd) handleNomadAllocationEvent(allocation *nomad.Allocation) error {
	if !allocation.ClientTerminalStatus() {
		cmd.logger.Printf("Ignoring allocation event with non-terminal client status \"%s\"", allocation.ClientStatus)
		return nil
	}

	action := &model.ActionInstance{}
	if err := pgxscan.Get(
		context.Background(), DB, action,
		`SELECT * FROM action_instances WHERE id = $1`,
		allocation.JobID,
	); err != nil {
		if pgxscan.NotFound(err) {
			cmd.logger.Printf("Ignoring Nomad event for Job with ID \"%s\" (no such action instance)\n", allocation.JobID)
			return nil
		}
		return errors.WithMessage(err, "Error finding action instance for Nomad event's Job")
	}

	var certs *model.WorkflowCerts

	def, err := GetDefinitionByAction(action, cmd.logger, cmd.evaluator)
	if err != nil {
		return errors.WithMessagef(err, "Could not get definition for action instance %s", action.ID)
	}

	switch allocation.ClientStatus {
	case "complete":
		certs = &def.Success
	case "failed":
		certs = &def.Failure
	}

	if certs == nil {
		return nil
	}

	modifyTime := time.Unix(
		allocation.ModifyTime/int64(time.Second),
		allocation.ModifyTime%int64(time.Second),
	).UTC()
	action.FinishedAt = &modifyTime

	wf, err := GetWorkflow(action)
	if err != nil {
		return err
	}

	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if _, err := tx.Exec(
			context.Background(),
			`UPDATE action_instances SET finished_at = $2 WHERE id = $1`,
			action.ID, action.FinishedAt,
		); err != nil {
			return errors.WithMessage(err, "Could not update action instance")
		}

		if err := service.Publish(
			cmd.logger,
			cmd.bridge,
			fmt.Sprintf("workflow.%s.%d.cert", wf.Name, wf.ID),
			"workflow.*.*.cert",
			*certs,
		); err != nil {
			return errors.WithMessage(err, "Could not publish certificate")
		}

		return nil
	}); err != nil {
		return errors.WithMessage(err, "Could not complete db transaction")
	}

	return nil
}
