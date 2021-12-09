package component

import (
	"context"
	"github.com/input-output-hk/cicero/src/application/mocks"
	configMocks "github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/stretchr/testify/assert"
	"log"
	"os"
	"testing"
)

func buildWorkflowStartConsumerMocked(messageQueueService *mocks.MessageQueueService,
	workflowService *mocks.WorkflowService) *WorkflowStartConsumer {
	return &WorkflowStartConsumer{
		Logger:              log.New(os.Stderr, "WorkflowFactConsumerTest: ", log.LstdFlags),
		MessageQueueService: messageQueueService,
		WorkflowService:     workflowService,
	}
}

func TestGettingWorkflowDetailFailure(t *testing.T) {
	t.Parallel()

	// given
	msg := &liftbridge.Message{}
	workflowFactConsumer := buildWorkflowStartConsumerMocked(nil, nil)

	// when
	_, err := workflowFactConsumer.getWorkflowDetailToProcess(msg)

	// then
	assert.Equal(t, err.Error(), "Invalid Message received, ignoring: ")
}

func TestProcessWorkflowToStartSuccess(t *testing.T) {
	t.Parallel()

	// given
	message := &liftbridge.Message{}
	tx := configMocks.BuildTransaction(context.Background(), t)
	wInstance := &domain.WorkflowInstance{
		ID:    uint64(1),
		Name:  "name",
		Facts: domain.Facts{},
	}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := &mocks.WorkflowService{}
	workflowFactConsumer := buildWorkflowStartConsumerMocked(messageQueueService, workflowService)
	messageQueueService.On("Save", tx, message).Return(nil)
	workflowService.On("Save", tx, wInstance).Return(nil)
	messageQueueService.On("Publish",
		domain.InvokeStreamName.Fmt(wInstance.Name, wInstance.ID),
		domain.InvokeStreamName,
		wInstance.Facts).Return(nil)

	// when
	err := workflowFactConsumer.processMessage(tx, wInstance, message)

	// then
	assert.Nil(t, err)
	workflowService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
}
