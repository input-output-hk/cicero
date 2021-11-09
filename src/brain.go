package cicero

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/google/uuid"
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
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	Evaluator      string `arg:"--evaluator" default:"cicero-evaluator-nix"`
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
	if brain.bridge == nil {
		if bridge, err := service.LiftbridgeConnect(self.LiftbridgeAddr); err != nil {
			brain.logger.Fatalln(err.Error())
			return
		} else {
			brain.bridge = &bridge
		}
	}
	if brain.workflowService == nil {
		s := service.NewWorkflowService(DB, *brain.bridge)
		brain.workflowService = &s
	}
	if brain.actionService == nil {
		s := service.NewActionService(DB)
		brain.actionService = &s
	}
	if brain.evaluator == nil {
		e := NewEvaluator(self.Evaluator)
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
	logger          *log.Logger
	tree            *oversight.Tree
	bridge          *liftbridge.Client
	workflowService *service.WorkflowService
	actionService   *service.ActionService
	evaluator       *Evaluator
}

func (self *Brain) addToTree(tree *oversight.Tree) {
	tree.Add(self.listenToCerts)
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

	err := service.CreateStreams(self.logger, *self.bridge, []string{service.StartStreamName})
	if err != nil {
		return err
	}

	var offset int64
	pgxscan.Get(context.Background(), DB, &offset,
		`SELECT COALESCE(MAX("offset") + 1, 0) FROM liftbridge_messages WHERE stream = $1`,
		service.StartStreamName)

	self.logger.Printf("Subscribing to %s at offset %d\n", service.StartStreamName, offset)
	err = (*self.bridge).Subscribe(ctx, service.StartStreamName, self.onStartMessage,
		liftbridge.StartAtOffset(offset), liftbridge.Partition(0),
	)

	if err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", service.StartStreamName)
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

	received := model.WorkflowCerts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
		return
	}

	if err := service.InsertLiftbridgeMessage(self.logger, DB, msg); err != nil {
		return
	}

	var version string
	if versionFromMsg, versionGiven := msg.Headers()["version"]; versionGiven {
		version = string(versionFromMsg)
	} else if def, err := self.evaluator.EvaluateWorkflow(workflowName, nil, 0, model.WorkflowCerts{}); err != nil {
		self.logger.Printf("Could not evaluate workflow %s to get latest version", workflowName)
		return
	} else {
		version = def.Version
	}

	workflow := model.WorkflowInstance{
		Name:    workflowName,
		Version: version,
		Certs:   received,
	}

	//TODO: FIXME this context to be transactional
	if err = self.insertWorkflow(context.Background(), &workflow); err != nil {
		self.logger.Printf("Failed to insert new workflow: %s\n", err)
	}
}

func (self *Brain) insertWorkflow(ctx context.Context, workflow *model.WorkflowInstance) error {
	var tx pgx.Tx
	if t, err := DB.Begin(ctx); err != nil {
		self.logger.Printf("%s\n", err)
		return err
	} else {
		tx = t
	}

	defer tx.Rollback(ctx)

	if err := (*self.workflowService).Save(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	self.logger.Printf("Created workflow with ID %d\n", workflow.ID)

	service.Publish(
		self.logger,
		*self.bridge,
		fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID),
		service.InvokeStreamName,
		workflow.Certs,
	)

	if err := tx.Commit(context.Background()); err != nil {
		self.logger.Printf("Couldn't complete transaction: %s\n", err)
	}

	return nil
}

func (self *Brain) listenToCerts(ctx context.Context) error {
	self.logger.Println("Starting Brain.listenToCerts")

	if err := service.CreateStreams(self.logger, *self.bridge, []string{service.CertStreamName}); err != nil {
		return err
	}

	var offset int64
	pgxscan.Get(
		context.Background(), DB, &offset,
		`SELECT COALESCE(MAX("offset") + 1, 0) FROM liftbridge_messages WHERE stream = $1`,
		service.CertStreamName,
	)

	self.logger.Printf("Subscribing to %s at offset %d\n", service.CertStreamName, offset)
	if err := (*self.bridge).Subscribe(
		ctx,
		service.CertStreamName,
		self.onCertMessage,
		liftbridge.StartAtOffset(offset),
		liftbridge.Partition(0),
	); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", service.CertStreamName)
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Brain) onCertMessage(msg *liftbridge.Message, err error) {
	if err != nil {
		self.logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
	}

	parts := strings.Split(msg.Subject(), ".")
	workflowName := parts[1]
	id, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		self.logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
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

	received := model.WorkflowCerts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.logger.Printf("Invalid JSON received, ignoring: %s\n", unmarshalErr)
		return
	}

	ctx := context.Background()
	tx, err := DB.Begin(ctx)
	if err != nil {
		self.logger.Printf("%s\n", err)
		return
	}

	defer tx.Rollback(ctx)

	if err := service.InsertLiftbridgeMessage(self.logger, tx, msg); err != nil {
		return
	}

	wf, err := (*self.workflowService).GetById(id)
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

	if err := (*self.workflowService).Update(tx, id, *existing); err != nil {
		self.logger.Printf("Error while updating workflow: %s", err)
		return
	}

	// TODO: only invoke when there was a change to the certs?

	self.logger.Printf("Updated workflow %#v\n", existing)

	if err := service.Publish(
		self.logger,
		*self.bridge,
		fmt.Sprintf("workflow.%s.%d.invoke", workflowName, id),
		service.InvokeStreamName,
		merged,
	); err != nil {
		self.logger.Printf("Couldn't publish workflow invoke message: %s\n", err)
		return
	}

	if err = tx.Commit(context.Background()); err != nil {
		self.logger.Printf("Couldn't complete transaction: %s\n", err)
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

	action, err := (*self.actionService).GetById(uuid.MustParse(allocation.JobID))
	if err != nil {
		if pgxscan.NotFound(err) {
			self.logger.Printf("Ignoring Nomad event for Job with ID \"%s\" (no such action instance)\n", allocation.JobID)
			return nil
		}
		return err
	}

	def, err := GetDefinitionByAction(action, self.logger, *self.evaluator)
	if err != nil {
		return errors.WithMessagef(err, "Could not get definition for action instance %s", action.ID)
	}

	var certs *model.WorkflowCerts
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

	wf, err := (*self.workflowService).GetById(action.WorkflowInstanceId)
	if err != nil {
		return errors.WithMessagef(err, "Could not get workflow instance for action %s", action.ID)
	}

	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := (*self.actionService).Update(tx, action.ID, action); err != nil {
			return errors.WithMessage(err, "Could not update action instance")
		}

		if err := service.Publish(
			self.logger,
			*self.bridge,
			fmt.Sprintf("workflow.%s.%d.cert", wf.Name, wf.ID),
			service.CertStreamName,
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
