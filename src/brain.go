package cicero

import (
	"context"
	"encoding/json"
	"fmt"
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
	evaluator Evaluator
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

	err := createStreams(cmd.logger, cmd.bridge, []string{streamName})
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

			received := WorkflowCerts{}
			unmarshalErr := json.Unmarshal(msg.Value(), &received)
			if unmarshalErr != nil {
				cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
				return
			}

			if err = cmd.insertWorkflow(&WorkflowInstance{Name: workflowName, Certs: received}); err != nil {
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

func (cmd *BrainCmd) insertWorkflow(workflow *WorkflowInstance) error {
	err := DB.QueryRow(context.Background(),
		`INSERT INTO workflow_instances (name, certs) VALUES ($1, $2) RETURNING id`,
		workflow.Name, workflow.Certs).
		Scan(&workflow.ID)
	if err != nil {
		return err
	}

	if err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	cmd.logger.Printf("Created workflow with ID %d\n", workflow.ID)

	publish(
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

	err := createStreams(cmd.logger, cmd.bridge, []string{streamName})
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

			tx, err := DB.Begin(ctx)
			if err != nil {
				cmd.logger.Printf("%s\n", err)
				return
			}

			existing := &WorkflowInstance{Name: workflowName}
			err = pgxscan.Get(
				context.Background(), tx, existing,
				`SELECT * FROM workflow_instances WHERE id = $1`,
				id,
			)
			if err != nil {
				cmd.logger.Printf("Couldn't select existing workflow for id %d: %s\n", id, err)
				return
			}

			merged := WorkflowCerts{}

			for k, v := range existing.Certs {
				merged[k] = v
			}

			for k, v := range received {
				merged[k] = v
			}

			now := time.Now().UTC()
			existing.Certs = merged
			existing.UpdatedAt = &now

			_, err = tx.Exec(
				context.Background(),
				`UPDATE workflow_instances SET certs = $2, updated_at = $3 WHERE id = $1`,
				id, existing.Certs, now,
			)

			if err != nil {
				cmd.logger.Printf("Error while updating workflow: %s", err)
				return
			}

			// TODO: only invoke when there was a change to the certs?

			cmd.logger.Printf("Updated workflow %#v\n", existing)

			publish(
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
	err := DB.QueryRow(context.Background(),
		`SELECT COALESCE(MAX("index") + 1, 0) FROM nomad_events`).
		Scan(&index)
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
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

			_, err := DB.Exec(
				context.Background(),
				`INSERT INTO nomad_events (topic, "type", "key", filter_keys, "index", payload) VALUES ($1, $2, $3, $4, $5, $6)`,
				event.Topic, event.Type, event.Key, event.FilterKeys, event.Index, event.Payload,
			)
			if err != nil {
				return errors.WithMessage(err, "Could not insert Nomad event into database")
			}

			index = event.Index
		}
	}
}

func (cmd *BrainCmd) handleNomadEvent(event nomad.Event) error {
	switch {
	case event.Topic == "Job" && event.Type == "AllocationUpdated":
		job, err := event.Job()
		if err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's Job")
		}
		return cmd.handleNomadJobEvent(job)
	}
	return nil
}

func (cmd *BrainCmd) handleNomadJobEvent(job *nomad.Job) error {
	step := &StepInstance{}
	err := pgxscan.Get(
		context.Background(), DB, step,
		`SELECT * FROM step_instances WHERE id = $1`,
		job.ID,
	)
	if err != nil {
		if pgxscan.NotFound(err) {
			cmd.logger.Printf("Ignoring Nomad event for Job with ID \"%s\" (no such step instance)\n", *job.ID)
			return nil
		}
		return errors.WithMessage(err, "Error finding step instance for Nomad event's Job")
	}

	var certs *WorkflowCerts

	def, err := step.GetDefinition(cmd.logger, cmd.evaluator)
	if err != nil {
		return errors.WithMessagef(err, "Could not get definition for step instance %s", step.ID)
	}

	switch *job.Status {
	case "dead":
		certs = &def.Success
	case "failed":
		// FIXME there is no such state in topic:Job,type:AllocationUpdated events
		// in fact such events do not contain any information about whether the job failed or succeeded,
		// they just say whether it is running. => Failure cert needs to be sent on another event.
		certs = &def.Failure
	}

	if certs == nil {
		return nil
	}

	now := time.Now().UTC()
	step.FinishedAt = &now

	wf, err := step.GetWorkflow()
	if err != nil {
		return err
	}

	err = DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		_, err = tx.Exec(context.Background(),
			`UPDATE step_instances SET finished_at = $2 WHERE id = $1`,
			step.ID, step.FinishedAt)
		if err != nil {
			return errors.WithMessage(err, "Could not update step instance")
		}

		err = publish(
			cmd.logger,
			cmd.bridge,
			fmt.Sprintf("workflow.%s.%d.cert", wf.Name, wf.ID),
			"workflow.*.*.cert",
			*certs,
		)
		if err != nil {
			return errors.WithMessage(err, "Could not publish certificate")
		}

		return nil
	})
	if err != nil {
		return errors.WithMessage(err, "Could not complete db transaction")
	}

	return nil
}
