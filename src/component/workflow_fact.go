package component

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowFactConsumer struct {
	Logger              *log.Logger
	MessageQueueService application.MessageQueueService
	WorkflowService     application.WorkflowService
	Db                  *pgxpool.Pool
}

func (self *WorkflowFactConsumer) Start(ctx context.Context) error {
	self.Logger.Println("Starting WorkflowFactConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.FactStreamName, self.onFactMessage, 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.FactStreamName)
	}

	<-ctx.Done()
	self.Logger.Println("context was cancelled")
	return nil
}

func (self *WorkflowFactConsumer) onFactMessage(msg *liftbridge.Message, err error) {
	if err != nil {
		self.Logger.Printf("error received in %s: %s", msg.Stream(), err.Error())
	}

	parts := strings.Split(msg.Subject(), ".")
	workflowName := parts[1]
	id, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		self.Logger.Printf("Invalid Workflow ID received, ignoring: %s", msg.Subject())
		return
	}

	self.Logger.Printf("Received update for workflow %s %d", workflowName, id)
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

	ctx := context.Background()
	tx, err := self.Db.Begin(ctx)
	if err != nil {
		self.Logger.Printf("%s", err)
		return
	}

	defer tx.Rollback(ctx)

	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return
	}

	var existing domain.WorkflowInstance
	if existing, err = self.WorkflowService.GetById(id); err != nil {
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

	if err := self.WorkflowService.Update(tx, existing); err != nil {
		self.Logger.Printf("Error while updating workflow: %s", err)
		return
	}

	// TODO: only invoke when there was a change to the facts?

	self.Logger.Printf("Updated workflow name: %s id: %d", existing.Name, existing.ID)

	if err := self.MessageQueueService.Publish(
		domain.InvokeStreamName.Fmt(workflowName, id),
		domain.InvokeStreamName,
		merged,
	); err != nil {
		self.Logger.Printf("Couldn't publish workflow invoke message: %s", err)
		return
	}

	if err = tx.Commit(context.Background()); err != nil {
		self.Logger.Printf("Couldn't complete transaction: %s", err)
		return
	}
}
