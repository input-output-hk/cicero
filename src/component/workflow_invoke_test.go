package component

import (
	"context"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application/mocks"
	configMocks "github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/vivek-ng/concurrency-limiter/priority"
	"log"
	"os"
	"testing"
)

func buildWorkflowInvokeConsumerMocked(messageQueueService *mocks.MessageQueueService,
	workflowService *mocks.WorkflowService,
	evaluationService *mocks.EvaluationService,
	actionService *mocks.ActionService,
	nomadClient *mocks.NomadClient) *WorkflowInvokeConsumer {
	return &WorkflowInvokeConsumer{
		Logger:              log.New(os.Stderr, "WorkflowInvokeConsumerTest: ", log.LstdFlags),
		MessageQueueService: messageQueueService,
		WorkflowService:     workflowService,
		EvaluationService:   evaluationService,
		ActionService:       actionService,
		NomadClient:         nomadClient,
		Limiter:             priority.NewLimiter(1),
	}
}

func TestGettingWorflowDetailFailure(t *testing.T) {
	t.Parallel()

	// given
	msg := &liftbridge.Message{}
	workflowInvokeConsumer := buildWorkflowInvokeConsumerMocked(nil, nil, nil, nil, nil)

	// when
	_, err := workflowInvokeConsumer.getWorkflowDetails(msg)

	// then
	assert.Equal(t, err.Error(), "Invalid Message received, ignoring: ")
}

func TestProcessWorkflowWithDifferentNameFailure(t *testing.T) {
	t.Parallel()

	// given
	ctx := context.Background()
	message := &liftbridge.Message{}
	tx := configMocks.BuildTransaction(ctx, t)
	wId := uint64(1)
	wMessageDetail := &domain.WorkflowInstance{
		ID:    wId,
		Name:  "name",
		Facts: domain.Facts{},
	}
	wInstance := domain.WorkflowInstance{
		Name: "anotherName",
	}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := &mocks.WorkflowService{}
	workflowInvokeConsumer := buildWorkflowInvokeConsumerMocked(messageQueueService, workflowService, nil, nil, nil)
	messageQueueService.On("Save", tx, message).Return(nil)
	workflowService.On("GetById", wId).Return(wInstance, nil)

	// when
	err := workflowInvokeConsumer.processMessage(ctx, tx, wMessageDetail, message)

	// then
	assert.Equal(t, err.Error(), "Workflow name given does not match name of instance: name != anotherName")
	workflowService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
}

func TestProcessWorkflowWithoutActionsSuccess(t *testing.T) {
	t.Parallel()

	// given
	ctx := context.Background()
	message := &liftbridge.Message{}
	tx := configMocks.BuildTransaction(ctx, t)
	wId := uint64(1)
	wName := "name"
	wMessageDetail := &domain.WorkflowInstance{
		ID:    wId,
		Name:  wName,
		Facts: domain.Facts{},
	}
	wInstance := domain.WorkflowInstance{
		Name:   wName,
		Source: "source",
	}
	workflowDefinition := domain.WorkflowDefinition{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := &mocks.WorkflowService{}
	evaluateWorkflow := &mocks.EvaluationService{}
	workflowInvokeConsumer := buildWorkflowInvokeConsumerMocked(messageQueueService, workflowService, evaluateWorkflow, nil, nil)
	messageQueueService.On("Save", tx, message).Return(nil)
	workflowService.On("GetById", wId).Return(wInstance, nil)
	evaluateWorkflow.On("EvaluateWorkflow", wInstance.Source, wMessageDetail.Name, wMessageDetail.ID, wMessageDetail.Facts).Return(workflowDefinition, nil)

	// when
	err := workflowInvokeConsumer.processMessage(ctx, tx, wMessageDetail, message)

	// then
	assert.Nil(t, err)
	workflowService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
	evaluateWorkflow.AssertExpectations(t)
}

func TestInvokeWorkflowActionSuccess(t *testing.T) {
	t.Parallel()

	// given
	ctx := context.Background()
	tx := configMocks.BuildTransaction(ctx, t)
	wId := uint64(1)
	wFacts := domain.Facts{}
	actionName := "name"
	action := &domain.WorkflowAction{}
	actionInstance := domain.ActionInstance{
		ID: uuid.New(),
	}
	actionService := &mocks.ActionService{}
	nomadClient := &mocks.NomadClient{}
	workflowInvokeConsumer := buildWorkflowInvokeConsumerMocked(nil,
		nil,
		nil,
		actionService,
		nomadClient)
	actionService.On("GetByNameAndWorkflowId", actionName, wId).Return(actionInstance, nil)
	nomadClient.On("JobsDeregister", actionInstance.ID.String(),
		false, &nomad.WriteOptions{}).Return(
		"result", &nomad.WriteMeta{}, nil)
	actionService.On("Update", tx, mock.AnythingOfType("domain.ActionInstance")).Return(nil)

	// when
	err := workflowInvokeConsumer.invokeWorkflowAction(ctx, tx, wId, wFacts, actionName, action)

	// then
	assert.Nil(t, err)
	nomadClient.AssertExpectations(t)
	actionService.AssertExpectations(t)
}
