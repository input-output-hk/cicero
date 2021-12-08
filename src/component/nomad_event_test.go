package component

import (
	"context"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/stretchr/testify/mock"
	"log"
	"os"
	"testing"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application/mocks"
	configMocks "github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/stretchr/testify/assert"
)

func buildNomadEventConsumerMocked(nomadEventServiceMocked *mocks.NomadEventService,
	nomadClientMocked *mocks.NomadClient,
	db *configMocks.PgxIface,
	actionService *mocks.ActionService,
	workflowService *mocks.WorkflowService,
	evaluationService *mocks.EvaluationService,
	messageQueueService *mocks.MessageQueueService) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:              log.New(os.Stderr, "NomadEventConsumerTest: ", log.LstdFlags),
		NomadEventService:   nomadEventServiceMocked,
		NomadClient:         nomadClientMocked,
		ActionService:       actionService,
		WorkflowService:     workflowService,
		EvaluationService:   evaluationService,
		MessageQueueService: messageQueueService,
		Db:                  db,
	}
}

func generateEvents(events []nomad.Events) <-chan *nomad.Events {
	rc := make(chan *nomad.Events, len(events))
	go func() {
		defer close(rc)
		for _, event := range events {
			rc <- &event
		}
	}()
	return rc
}

func TestStartWorkflowFailureToListenNomadEvent(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	stream := generateEvents([]nomad.Events{})
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil, nil, nil, nil, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	errorMessage := "Some error"
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, errors.New(errorMessage))

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not listen to Nomad events: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureGettingNextEvents(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	events := []nomad.Events{
		{Err: errors.New(errorMessage)},
	}
	stream := generateEvents(events)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil, nil, nil, nil, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Error getting next events from Nomad event stream: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowProcessNomadEventFailure(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation"}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(events)

	db := &configMocks.PgxIface{}
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, db, nil, nil, nil, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)
	db.On("BeginFunc", ctx, mock.AnythingOfType("func(pgx.Tx) error")).Return(errors.New(errorMessage))

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), errorMessage)
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
	db.AssertExpectations(t)
}

func Test_sdf(t *testing.T) {
	t.Parallel()

	// given
	jobId := uuid.New()
	ctx := context.Background()
	event := nomad.Event{Topic: "Allocation", Type: "AllocationUpdated", Payload: map[string]interface{}{
		"Allocation": nomad.Allocation{
			ID:           "some-id",
			JobID:        jobId.String(),
			Namespace:    "some-namespace-id",
			ClientStatus: "complete",
		},
	}}
	workflowId := uint64(1)
	actionName := "action"
	action := domain.ActionInstance{WorkflowInstanceId: workflowId, Name: actionName}
	workflow := domain.WorkflowInstance{
		ID:     workflowId,
		Name:   "name",
		Source: "source",
		Facts:  domain.Facts{},
	}
	facts := domain.Facts{}
	workflowAction := domain.WorkflowAction{Success: facts}
	workflowDefinition := domain.WorkflowDefinition{
		Actions: map[string]*domain.WorkflowAction{actionName: &workflowAction},
	}

	tx := configMocks.BuildTransaction(ctx, t)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	actionService := &mocks.ActionService{}
	workflowService := &mocks.WorkflowService{}
	evaluationService := &mocks.EvaluationService{}
	messageQueueService := &mocks.MessageQueueService{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil, actionService, workflowService, evaluationService, messageQueueService)
	actionService.On("GetById", jobId).Return(action, nil)
	workflowService.On("GetById", workflowId).Return(workflow, nil)
	evaluationService.On("EvaluateWorkflow", workflow.Source, workflow.Name, workflow.ID, workflow.Facts).Return(workflowDefinition, nil)
	workflowService.On("GetById", workflowId).Return(workflow, nil)
	actionService.On("Update", tx, mock.AnythingOfType("domain.ActionInstance")).Return(nil)
	messageQueueService.On("Publish",
		domain.FactStreamName.Fmt(workflow.Name, workflow.ID),
		domain.FactStreamName,
		facts,
	).Return(nil)
	nomadEventService.On("Save", tx, &event).Return(nil)

	// when
	err := nomadEventConsumer.processNomadEvent(&event, tx)

	// then
	assert.Nil(t, err)
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
	actionService.AssertExpectations(t)
	workflowService.AssertExpectations(t)
	evaluationService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
}
