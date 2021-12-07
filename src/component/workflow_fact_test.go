package component

import (
	"context"
	"github.com/input-output-hk/cicero/src/application/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pashagolub/pgxmock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"log"
	"os"
	"testing"
)

func buildWorkflowFactConsumerMocked(messageQueueService *mocks.MessageQueueService,
	workflowService *mocks.WorkflowService) *WorkflowFactConsumer {
	return &WorkflowFactConsumer{
		Logger:              log.New(os.Stderr, "WorkflowFactConsumerTest: ", log.LstdFlags),
		MessageQueueService: messageQueueService,
		WorkflowService:     workflowService,
	}
}

func TestGettingFactDetailFailure(t *testing.T) {
	t.Parallel()

	// given
	msg := &liftbridge.Message{}
	workflowFactConsumer := buildWorkflowFactConsumerMocked(nil, nil)

	// when
	_, err := workflowFactConsumer.getFactsDetail(msg)

	// then
	assert.Equal(t, err.Error(), "Invalid Message received, ignoring: ")
}

func TestUpdatingFactsFailure(t *testing.T) {
	t.Parallel()

	// given
	ctx := context.Background()
	message := &liftbridge.Message{}
	db, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer db.Close(ctx)
	db.ExpectBegin()
	tx, err := db.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	defer func() { _ = tx.Rollback(ctx) }()
	wId := uint64(1)
	wMessageDetail := &domain.WorkflowInstance{
		ID:    wId,
		Name:  "name",
		Facts: domain.Facts{},
	}
	wInstance := domain.WorkflowInstance{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := &mocks.WorkflowService{}
	workflowFactConsumer := buildWorkflowFactConsumerMocked(messageQueueService, workflowService)
	messageQueueService.On("Save", tx, message).Return(nil)
	workflowService.On("GetById", wId).Return(wInstance, nil)
	errorMessage := "Some error"
	workflowService.On("Update", tx, mock.AnythingOfType("domain.WorkflowInstance")).Return(errors.New(errorMessage))

	// when
	err = workflowFactConsumer.processMessage(tx, wMessageDetail, message)

	// then
	assert.Equal(t, err.Error(), errorMessage)
	workflowService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
}

func TestProcessFactsSuccess(t *testing.T) {
	t.Parallel()

	// given
	ctx := context.Background()
	message := &liftbridge.Message{}
	db, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer db.Close(ctx)
	db.ExpectBegin()
	tx, err := db.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	defer func() { _ = tx.Rollback(ctx) }()
	wId := uint64(1)
	wMessageDetail := &domain.WorkflowInstance{
		ID:    wId,
		Name:  "name",
		Facts: domain.Facts{},
	}
	wInstance := domain.WorkflowInstance{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := &mocks.WorkflowService{}
	workflowFactConsumer := buildWorkflowFactConsumerMocked(messageQueueService, workflowService)
	messageQueueService.On("Save", tx, message).Return(nil)
	workflowService.On("GetById", wId).Return(wInstance, nil)
	workflowService.On("Update", tx, mock.AnythingOfType("domain.WorkflowInstance")).Return(nil)
	messageQueueService.On("Publish",
		domain.InvokeStreamName.Fmt(wMessageDetail.Name, wMessageDetail.ID),
		domain.InvokeStreamName,
		wMessageDetail.Facts).Return(nil)

	// when
	err = workflowFactConsumer.processMessage(tx, wMessageDetail, message)

	// then
	assert.Nil(t, err)
	workflowService.AssertExpectations(t)
	messageQueueService.AssertExpectations(t)
}
