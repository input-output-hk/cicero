package cicero

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"cirello.io/oversight"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type BrainCmd struct {
	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (self BrainCmd) init(brain *Brain, db *pgxpool.Pool, nomadClient *nomad.Client) {
	if brain.db == nil {
		brain.db = db
	}
	if brain.nomadClient == nil {
		brain.nomadClient = nomadClient
	}
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
		s := application.NewWorkflowActionService(*brain.evaluator, brain.workflowService)
		brain.workflowActionService = s
	}
	if brain.messageQueueService == nil {
		if bridge, err := application.LiftbridgeConnect(self.LiftbridgeAddr); err != nil {
			brain.logger.Fatalln(err.Error())
			return
		} else {
			s := application.NewMessageQueueService(db, bridge)
			brain.messageQueueService = s
		}
	}
	if brain.workflowService == nil {
		s := application.NewWorkflowService(db, brain.messageQueueService)
		brain.workflowService = s
	}
	if brain.actionService == nil {
		s := application.NewActionService(db, self.PrometheusAddr)
		brain.actionService = s
	}
	if brain.nomadEventService == nil {
		s := application.NewNomadEventService(db, brain.actionService)
		brain.nomadEventService = s
	}
	if brain.evaluator == nil {
		e := application.NewEvaluator(self.Evaluator, self.Env)
		brain.evaluator = &e
	}
}

func (self BrainCmd) Run(db *pgxpool.Pool, nomadClient *nomad.Client) error {
	brain := Brain{}
	self.init(&brain, db, nomadClient)

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
	workflowService       application.WorkflowService
	actionService         application.ActionService
	workflowActionService application.WorkflowActionService
	messageQueueService   application.MessageQueueService
	nomadEventService     application.NomadEventService
	evaluator             *application.Evaluator
	db                    *pgxpool.Pool
	nomadClient           *nomad.Client
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

	if err := self.messageQueueService.Subscribe(ctx, domain.StartStreamName, self.onStartMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.StartStreamName)
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

	received := domain.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s", unmarshalErr)
		return
	}

	//TODO: must be transactional with the message process
	if err := (*self.db).BeginFunc(context.Background(), func(tx pgx.Tx) error {
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

	workflow := domain.WorkflowInstance{
		Name:   workflowName,
		Source: source,
		Facts:  received,
	}

	//TODO: FIXME this context to be transactional
	if err = self.insertWorkflow(context.Background(), &workflow); err != nil {
		self.logger.Printf("Failed to insert new workflow: %s", err)
	}
}

func (self *Brain) insertWorkflow(ctx context.Context, workflow *domain.WorkflowInstance) error {
	var tx pgx.Tx
	if t, err := (*self.db).Begin(ctx); err != nil {
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
		domain.InvokeStreamName.Fmt(workflow.Name, workflow.ID),
		domain.InvokeStreamName,
		workflow.Facts,
	)

	if err := tx.Commit(context.Background()); err != nil {
		self.logger.Printf("Couldn't complete transaction: %s", err)
	}

	return nil
}

func (self *Brain) listenToFacts(ctx context.Context) error {
	self.logger.Println("Starting Brain.listenToFacts")

	if err := self.messageQueueService.Subscribe(ctx, domain.FactStreamName, self.onFactMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.FactStreamName)
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

	received := domain.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s", unmarshalErr)
		return
	}

	ctx := context.Background()
	tx, err := (*self.db).Begin(ctx)
	if err != nil {
		self.logger.Printf("%s", err)
		return
	}

	defer tx.Rollback(ctx)

	if err := self.messageQueueService.Save(tx, msg); err != nil {
		return
	}

	var existing domain.WorkflowInstance
	if existing, err = self.workflowService.GetById(id); err != nil {
		return
	}

	merged := domain.Facts{}

	for k, v := range existing.Facts {
		merged[k] = v
	}

	for k, v := range received {
		merged[k] = v
	}

	now := time.Now().UTC()
	existing.Facts = merged
	existing.UpdatedAt = &now

	if err := self.workflowService.Update(tx, existing); err != nil {
		self.logger.Printf("Error while updating workflow: %s", err)
		return
	}

	// TODO: only invoke when there was a change to the facts?

	self.logger.Printf("Updated workflow %#v", existing)

	if err := self.messageQueueService.Publish(
		domain.InvokeStreamName.Fmt(workflowName, id),
		domain.InvokeStreamName,
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

	index, err := self.nomadEventService.GetLastNomadEvent()
	if err != nil && !errors.Is(err, pgx.ErrNoRows) {
		return errors.WithMessage(err, "Could not get last Nomad event index")
	}
	index += 1

	self.logger.Println("Listening to Nomad events starting at index", index)

	stream, err := self.nomadClient.EventStream().Stream(
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

		if events.Index < index {
			// We always get the last event even if we start at
			// an index greater than the last so we have to ignore it.
			// https://github.com/hashicorp/nomad/issues/11296
			continue
		}

		for _, event := range events.Events {
			if err := self.handleNomadEvent(&event); err != nil {
				return errors.WithMessage(err, "Error handling Nomad event")
			}

			//TODO: must be transactional with the message process
			if err := (*self.db).BeginFunc(context.Background(), func(tx pgx.Tx) error {
				return self.nomadEventService.Save(tx, &event)
			}); err != nil {
				self.logger.Println("Could not complete db transaction")
				return err
			}
		}

		index = events.Index
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
		// TODO We don't want to crash here (for example if a workflow source disappeared)
		// but we don't want to silently ignore it either - should this pop up in the web UI somewhere?
		// Unfortunately evaluators can currently not indicate the reason why evaluation failed - add that to contract?
		self.logger.Printf("Could not get definition for action instance %s: %s", action.ID, err.Error())
		return nil
	}

	var facts *domain.Facts
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

	if err := (*self.db).BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.actionService.Update(tx, action); err != nil {
			return errors.WithMessage(err, "Could not update action instance")
		}

		if err := self.messageQueueService.Publish(
			domain.FactStreamName.Fmt(wf.Name, wf.ID),
			domain.FactStreamName,
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
