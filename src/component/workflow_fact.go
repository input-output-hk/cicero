package component

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/jackc/pgx/v4"
	"github.com/rs/zerolog"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowFactConsumer struct {
	Logger              zerolog.Logger
	MessageQueueService application.MessageQueueService
	WorkflowService     application.WorkflowService
	Db                  config.PgxIface
}

func (self *WorkflowFactConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting...")

	if err := self.MessageQueueService.Subscribe(ctx, domain.FactStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.FactStreamName)
	}

	<-ctx.Done()
	self.Logger.Info().Msg("context was cancelled")
	return nil
}

func (self *WorkflowFactConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatal().Err(err).Msgf("the subscription %s will be terminated", domain.FactStreamName)
			//TODO: If err is not nil, the subscription will be terminated
		}
		self.Logger.Debug().Msgf("Processing message - stream: %s - subject: %s - offset: %d - value: %s, time: %s",
			msg.Stream(), msg.Subject(), msg.Offset(), string(msg.Value()), msg.Timestamp(),
		)
		wMessageDetail, err := self.getFactsDetail(msg)
		if err != nil {
			self.Logger.Error().Err(err).Msgf("Invalid Workflow received, ignoring: %s", msg.Subject())
			return
		}
		self.Logger.Info().Msgf("Received update for workflow with ID: %d, NAME: %s, FACTS: %v", wMessageDetail.ID, wMessageDetail.Name, wMessageDetail.Facts)

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(tx, wMessageDetail, msg)
		}); err != nil {
			self.Logger.Error().Err(err).Msgf("Could not process fact message: %v", msg)
			return
		}

		self.Logger.Info().Msgf("Updated workflow with ID %d", wMessageDetail.ID)
	}
}

func (self *WorkflowFactConsumer) getFactsDetail(msg *liftbridge.Message) (*domain.WorkflowInstance, error) {
	parts := strings.Split(msg.Subject(), ".")
	if len(parts) < 3 {
		return nil, fmt.Errorf("Invalid Message received, ignoring: %s", msg.Subject())
	}
	workflowName := parts[1]
	id, err := strconv.ParseUint(parts[2], 10, 64)
	if err != nil {
		return nil, errors.WithMessagef(err, "Invalid Workflow ID received, ignoring: %s", msg.Subject())
	}
	received := domain.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		return nil, errors.Errorf("Invalid JSON received, ignoring: %s", unmarshalErr)
	}
	return &domain.WorkflowInstance{
		ID:    id,
		Name:  workflowName,
		Facts: received,
	}, nil
}

func (self *WorkflowFactConsumer) processMessage(tx pgx.Tx, workflow *domain.WorkflowInstance, msg *liftbridge.Message) (err error) {
	if err = self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessagef(err, "Could not save message event %v", msg)
	}
	var existing domain.WorkflowInstance
	if existing, err = self.WorkflowService.GetById(workflow.ID); err != nil {
		return err
	}
	merged := domain.Facts{}

	for k, v := range existing.Facts {
		merged[k] = v
	}

	for k, v := range workflow.Facts {
		merged[k] = v
	}

	now := time.Now().UTC()
	existing.Facts = merged
	existing.UpdatedAt = &now

	if err := self.WorkflowService.Update(tx, existing); err != nil {
		return errors.WithMessagef(err, "Error while updating workflow in msg %v", msg)
	}

	// TODO: only invoke when there was a change to the facts?
	if err := self.MessageQueueService.Publish(
		domain.InvokeStreamName.Fmt(workflow.Name, workflow.ID),
		domain.InvokeStreamName,
		merged,
	); err != nil {
		return errors.WithMessagef(err, "Couldn't publish workflow invoke message: %v", msg)
	}

	return nil
}
