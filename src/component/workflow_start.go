package component

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"strings"

	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowStartConsumer struct {
	Logger              *log.Logger
	MessageQueueService application.MessageQueueService
	WorkflowService     application.WorkflowService
	Db                  *pgxpool.Pool
}

func (self *WorkflowStartConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting WorkflowStartConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.StartStreamName, self.onStartMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.StartStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

func (self *WorkflowStartConsumer) onStartMessage(msg *liftbridge.Message, err error) {
	if err != nil {
		self.Logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
	}

	parts := strings.Split(msg.Subject(), ".")
	workflowName := parts[1]

	self.Logger.Printf("Received start for workflow %s", workflowName)
	self.Logger.Println(
		"stream:", msg.Stream(),
		"subject:", msg.Subject(),
		"offset:", msg.Offset(),
		"value:", string(msg.Value()),
		"time:", msg.Timestamp(),
	)

	received := domain.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		self.Logger.Printf("Invalid JSON received, ignoring: %s", unmarshalErr)
		return
	}

	//TODO: must be transactional with the message process
	if err := self.Db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		err := self.MessageQueueService.Save(tx, msg)
		return err
	}); err != nil {
		self.Logger.Printf("Could not complete db transaction")
		return
	}

	var source string
	if sourceFromMsg, sourceGiven := msg.Headers()["source"]; !sourceGiven {
		self.Logger.Printf("No source given for workflow %s", workflowName)
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
		self.Logger.Printf("Failed to insert new workflow: %s", err)
	}
}

func (self *WorkflowStartConsumer) insertWorkflow(ctx context.Context, workflow *domain.WorkflowInstance) error {
	var tx pgx.Tx
	if t, err := self.Db.Begin(ctx); err != nil {
		self.Logger.Printf("%s", err)
		return err
	} else {
		tx = t
	}

	defer tx.Rollback(ctx)

	if err := self.WorkflowService.Save(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	self.Logger.Printf("Created workflow with ID %d", workflow.ID)

	self.MessageQueueService.Publish(
		domain.InvokeStreamName.Fmt(workflow.Name, workflow.ID),
		domain.InvokeStreamName,
		workflow.Facts,
	)

	if err := tx.Commit(context.Background()); err != nil {
		self.Logger.Printf("Couldn't complete transaction: %s", err)
	}

	return nil
}
