package component

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/rs/zerolog"
	"strings"

	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowStartConsumer struct {
	Logger              zerolog.Logger
	MessageQueueService application.MessageQueueService
	WorkflowService     application.WorkflowService
	Db                  config.PgxIface
}

func (self *WorkflowStartConsumer) Start(ctx context.Context) error {
	self.Logger.Info().Msg("Starting WorkflowStartConsumer")

	if err := self.MessageQueueService.Subscribe(ctx, domain.StartStreamName, self.invokerSubscriber(ctx), 0); err != nil {
		return errors.WithMessagef(err, "Couldn't subscribe to stream %s", domain.StartStreamName)
	}

	<-ctx.Done()
	self.Logger.Info().Msg("context was cancelled")
	return nil
}

func (self *WorkflowStartConsumer) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.Logger.Fatal().Err(err).Msgf("the subscription %s will be terminated", domain.StartStreamName)
			//TODO: If err is not nil, the subscription will be terminated
		}
		self.Logger.Debug().Msgf("Processing message",
			"stream:", msg.Stream(),
			"subject:", msg.Subject(),
			"offset:", msg.Offset(),
			"value:", string(msg.Value()),
			"time:", msg.Timestamp(),
		)
		workflowDetail, err := self.getWorkflowDetailToProcess(msg)
		if err != nil {
			self.Logger.Error().Err(err).Msgf("Invalid Workflow ID received, ignoring: %s", msg.Subject())
			return
		}
		self.Logger.Info().Msgf("Received start for workflow with NAME: %s, SOURCE: %v, FACTS: %v", workflowDetail.Name, workflowDetail.Source, workflowDetail.Facts)

		if err := self.Db.BeginFunc(ctx, func(tx pgx.Tx) error {
			return self.processMessage(tx, workflowDetail, msg)
		}); err != nil {
			self.Logger.Error().Err(err).Msgf("Could not process fact message: %v", msg)
			return
		}
		self.Logger.Info().Msgf("Created workflow with ID %d", workflowDetail.ID)
	}
}

func (self *WorkflowStartConsumer) getWorkflowDetailToProcess(msg *liftbridge.Message) (*domain.WorkflowInstance, error) {
	parts := strings.Split(msg.Subject(), ".")
	if len(parts) < 2 {
		return nil, fmt.Errorf("Invalid Message received, ignoring: %s", msg.Subject())
	}
	workflowName := parts[1]
	received := domain.Facts{}
	unmarshalErr := json.Unmarshal(msg.Value(), &received)
	if unmarshalErr != nil {
		return nil, errors.Errorf("Invalid JSON received, ignoring: %s", unmarshalErr)
	}
	var source string
	if sourceFromMsg, sourceGiven := msg.Headers()["source"]; !sourceGiven {
		return nil, errors.Errorf("No source given for workflow %s", workflowName)
	} else {
		source = string(sourceFromMsg)
	}
	return &domain.WorkflowInstance{
		Name:   workflowName,
		Source: source,
		Facts:  received,
	}, nil
}

func (self *WorkflowStartConsumer) processMessage(tx pgx.Tx, workflow *domain.WorkflowInstance, msg *liftbridge.Message) error {
	if err := self.MessageQueueService.Save(tx, msg); err != nil {
		return errors.WithMessagef(err, "Could not save message event %v", msg)
	}

	if err := self.WorkflowService.Save(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not insert workflow instance")
	}

	if err := self.MessageQueueService.Publish(
		domain.InvokeStreamName.Fmt(workflow.Name, workflow.ID),
		domain.InvokeStreamName,
		workflow.Facts,
	); err != nil {
		return err
	}
	return nil
}
