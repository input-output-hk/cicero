package component

import (
	"context"
	"github.com/input-output-hk/cicero/src/application/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pashagolub/pgxmock"
	"github.com/stretchr/testify/assert"
	"log"
	"os"
	"testing"
)

func buildWorkflowFactConsumerMocked(messageQueueService *mocks.MessageQueueService) *WorkflowFactConsumer {
	return &WorkflowFactConsumer{
		Logger:              log.New(os.Stderr, "WorkflowFactConsumerTest: ", log.LstdFlags),
		MessageQueueService: messageQueueService,
	}
}

func TestGettingWorkflowMessageDetailFailure(t *testing.T) {
	t.Parallel()

	//given
	msg := &liftbridge.Message{}
	workflowFactConsumer := buildWorkflowFactConsumerMocked(nil)

	//when
	_, err := workflowFactConsumer.getWorkflowMessageDetail(msg)

	//then
	assert.Equal(t, err.Error(), "Invalid Message received, ignoring: ")
}

func TestParsingWorkflowMessageDetailFailure(t *testing.T) {
	t.Parallel()

	//given
	ctx := context.Background()
	message := &liftbridge.Message{}
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer mock.Close(ctx)
	mock.ExpectBegin()
	tx, err := mock.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	defer tx.Rollback(ctx)
	wMessageDetail := &WorkflowMessageDetail{
		Id:    uint64(1),
		Name:  "name",
		Facts: domain.Facts{},
	}
	messageQueueService := &mocks.MessageQueueService{}
	workflowFactConsumer := buildWorkflowFactConsumerMocked(messageQueueService)
	messageQueueService.On("Save", tx, message).Return(nil)

	//when
	err = workflowFactConsumer.processMessage(tx, wMessageDetail, message)

	//then
	assert.Equal(t, err.Error(), "Invalid Message received, ignoring: ")
}
