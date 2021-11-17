package cicero

import (
	"context"
	"encoding/json"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"cirello.io/oversight"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type BrainCmd struct {
	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (self BrainCmd) init(brain *Brain) {
	if brain.logger == nil {
		brain.logger = log.New(os.Stderr, "brain: ", log.LstdFlags)
	}
	if brain.tree == nil {
		brain.tree = oversight.New(oversight.WithSpecification(
			10,                    // number of restarts
			10*time.Minute,        // within this time period
			oversight.OneForOne(), // restart every task on its own
		))
	}
	if brain.workflowActionService == nil {
		s := NewWorkflowActionService(*brain.evaluator, brain.workflowService)
		brain.workflowActionService = s
	}
	if brain.messageQueueService == nil {
		if bridge, err := service.LiftbridgeConnect(self.LiftbridgeAddr); err != nil {
			brain.logger.Fatalln(err.Error())
			return
		} else {
			s := service.NewMessageQueueService(DB, bridge)
			brain.messageQueueService = s
		}
	}
	if brain.workflowService == nil {
		s := service.NewWorkflowService(DB, brain.messageQueueService)
		brain.workflowService = s
	}
	if brain.actionService == nil {
		s := service.NewActionService(DB, self.PrometheusAddr)
		brain.actionService = s
	}
	if brain.evaluator == nil {
		e := NewEvaluator(self.Evaluator, self.Env)
		brain.evaluator = &e
	}
}

func (self BrainCmd) Run() error {
	brain := Brain{}
	self.init(&brain)

	if err := brain.start(context.Background()); err != nil {
		return errors.WithMessage(err, "While running brain")
	}

	for {
		time.Sleep(time.Hour)
	}
}

type Brain struct {
	logger                *log.Logger
	tree                  *oversight.Tree
	workflowService       service.WorkflowService
	actionService         service.ActionService
	workflowActionService WorkflowActionService
	messageQueueService   service.MessageQueueService
	evaluator             *Evaluator
}

func (self *Brain) addToTree(tree *oversight.Tree) {
	tree.Add(self.listenToFacts)
	tree.Add(self.listenToStart)
	tree.Add(self.listenToNomadEvents)
}

func (self *Brain) start(ctx context.Context) error {
	self.addToTree(self.tree)

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if err := self.tree.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting brain supervisor")
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Brain) listenToStart(ctx context.Context) error {
	self.logger.Println("Starting Brain.listenToStart")

	if err := self.messageQueueService.Subscribe(ctx, model.StartStreamName, self.onStartMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", model.StartStreamName)
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Brain) onStartMessage(msg *liftbridge.Message, err error) {
	if err != nil {
		self.logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
	}

	parts := strings.Split(msg.Subject(), ".")
	workflowName := parts[1]

	self.logger.Printf("Received start for workflow %s", workflowName)
	self.logger.Println(
		"stream:", msg.Stream(),
		"subject:", msg.Subject(),
		"offset:", msg.Offset(),
		"value:", string(msg.Value()),
		"time:", msg.Timestamp(),
	)

	received := model.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s", unmarshalErr)
		return
	}

	//TODO: must be transactional with the message process
	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		err := self.messageQueueService.Save(tx, msg)
		return err
	}); err != nil {
		self.logger.Printf("Could not complete db transaction")
		return
	}

	var source string
	if sourceFromMsg, sourceGiven := msg.Headers()["source"]; !sourceGiven {
		self.logger.Printf("No source given for workflow %s", workflowName)
		return
	} else {
		source = string(sourceFromMsg)
	}

	workflow := model.WorkflowInstance{
		Name:   workflowName,
		Source: source,
		Facts:  received,
	}

	//TODO: FIXME this context to be transactional
	if err = self.insertWorkflow(context.Background(), &workflow); err != nil {
		self.logger.Printf("Failed to insert new workflow: %s", err)
	}
}

func (self *Brain) insertWorkflow(ctx context.Context, workflow *model.WorkflowInstance) error {
	var tx pgx.Tx
	if t, err := DB.Begin(ctx); err != nil {
		self.logger.Printf("%s", err)
		return err
	} else {
		tx = t
	}

	defer tx.Rollback(ctx)

	if err := self.workflowService.Save(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	self.logger.Printf("Created workflow with ID %d", workflow.ID)

	self.messageQueueService.Publish(
		model.InvokeStreamName.Fmt(workflow.Name, workflow.ID),
		model.InvokeStreamName,
		workflow.Facts,
	)

	if err := tx.Commit(context.Background()); err != nil {
		self.logger.Printf("Couldn't complete transaction: %s", err)
	}

	return nil
}

func (self *Brain) listenToFacts(ctx context.Context) error {
	self.logger.Println("Starting Brain.listenToFacts")

	if err := self.messageQueueService.Subscribe(ctx, model.FactStreamName, self.onFactMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", model.FactStreamName)
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Brain) onFactMessage(msg *liftbridge.Message, err error) {
	if err != nil {
		self.logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
	}

	parts := strings.Split(msg.Subject(), ".")
	workflowName := parts[1]
	id, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		self.logger.Printf("Invalid Workflow ID received, ignoring: %s", msg.Subject())
		return
	}

	self.logger.Printf("Received update for workflow %s %d", workflowName, id)
	self.logger.Println(
		"stream:", msg.Stream(),
		"subject:", msg.Subject(),
		"offset:", msg.Offset(),
		"value:", string(msg.Value()),
		"time:", msg.Timestamp(),
	)

	received := model.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s", unmarshalErr)
		return
	}

	ctx := context.Background()
	tx, err := DB.Begin(ctx)
	if err != nil {
		self.logger.Printf("%s", err)
		return
	}

	defer tx.Rollback(ctx)

	if err := self.messageQueueService.Save(tx, msg); err != nil {
		return
	}

	wf, err := self.workflowService.GetById(id)
	var existing = &wf

	if err != nil {
		return
	}

	merged := model.Facts{}

	for k, v := range existing.Facts {
		merged[k] = v
	}

	for k, v := range received {
		merged[k] = v
	}

	now := time.Now().UTC()
	existing.Facts = merged
	existing.UpdatedAt = &now

	if err := self.workflowService.Update(tx, id, *existing); err != nil {
		self.logger.Printf("Error while updating workflow: %s", err)
		return
	}

	// TODO: only invoke when there was a change to the facts?

	self.logger.Printf("Updated workflow %#v", existing)

	if err := self.messageQueueService.Publish(
		model.InvokeStreamName.Fmt(workflowName, id),
		model.InvokeStreamName,
		merged,
	); err != nil {
		self.logger.Printf("Couldn't publish workflow invoke message: %s", err)
		return
	}

	if err = tx.Commit(context.Background()); err != nil {
		self.logger.Printf("Couldn't complete transaction: %s", err)
		return
	}
}

func (self *Brain) listenToNomadEvents(ctx context.Context) error {
	self.logger.Println("Starting Brain.listenToNomadEvents")

	var index uint64
	if err := DB.QueryRow(
		context.Background(),
		`SELECT COALESCE(MAX("index") + 1, 0) FROM nomad_events`,
	).Scan(&index); err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}

	self.logger.Println("Listening to Nomad events starting at index", index)

	stream, err := nomadClient.EventStream().Stream(
		ctx,
		map[nomad.Topic][]string{
			nomad.TopicDeployment: {string(nomad.TopicAll)},
			nomad.TopicEvaluation: {string(nomad.TopicAll)},
			nomad.TopicAllocation: {string(nomad.TopicAll)},
			nomad.TopicJob:        {string(nomad.TopicAll)},
			nomad.TopicNode:       {string(nomad.TopicAll)},
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
			if err := self.handleNomadEvent(&event); err != nil {
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

func (self *Brain) handleNomadEvent(event *nomad.Event) error {
	if event.Topic == "Allocation" && event.Type == "AllocationUpdated" {
		allocation, err := event.Allocation()
		if err != nil {
			return errors.WithMessage(err, "Error getting Nomad event's allocation")
		}
		return self.handleNomadAllocationEvent(allocation)
	}
	return nil
}

func (self *Brain) handleNomadAllocationEvent(allocation *nomad.Allocation) error {
	if !allocation.ClientTerminalStatus() {
		self.logger.Printf("Ignoring allocation event with non-terminal client status \"%s\"", allocation.ClientStatus)
		return nil
	}

	id, err := uuid.Parse(allocation.JobID)
	if err != nil {
		return nil
	}

	action, err := self.actionService.GetById(id)
	if err != nil {
		if pgxscan.NotFound(err) {
			self.logger.Printf("Ignoring Nomad event for Job with ID \"%s\" (no such action instance)", allocation.JobID)
			return nil
		}
		return err
	}

	def, err := self.workflowActionService.GetWorkflowAction(action)
	if err != nil {
		return errors.WithMessagef(err, "Could not get definition for action instance %s", action.ID)
	}

	var facts *model.Facts
	switch allocation.ClientStatus {
	case "complete":
		facts = &def.Success
	case "failed":
		facts = &def.Failure
	}
	if facts == nil {
		return nil
	}

	modifyTime := time.Unix(
		allocation.ModifyTime/int64(time.Second),
		allocation.ModifyTime%int64(time.Second),
	).UTC()
	action.FinishedAt = &modifyTime

	wf, err := self.workflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		return errors.WithMessagef(err, "Could not get workflow instance for action %s", action.ID)
	}

	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.actionService.Update(tx, action); err != nil {
			return errors.WithMessage(err, "Could not update action instance")
		}

		if err := self.messageQueueService.Publish(
			model.FactStreamName.Fmt(wf.Name, wf.ID),
			model.FactStreamName,
			*facts,
		); err != nil {
			return errors.WithMessage(err, "Could not publish fact")
		}

		return nil
	}); err != nil {
		return errors.WithMessage(err, "Could not complete db transaction")
	}

	return nil
}
