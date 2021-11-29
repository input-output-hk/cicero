package application

import (
	"context"
	"fmt"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pashagolub/pgxmock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"log"
	"os"
	"testing"
)


type WorkflowRepositoryMocked struct {
	mock.Mock
}

func (w *WorkflowRepositoryMocked) GetSummary() (domain.WorkflowSummary, error) {
	panic("implement me")
}

func (w *WorkflowRepositoryMocked) GetAll() ([]*domain.WorkflowInstance, error) {
	panic("implement me")
}

func (w *WorkflowRepositoryMocked) GetAllByName(s string) ([]*domain.WorkflowInstance, error) {
	panic("implement me")
}

func (w *WorkflowRepositoryMocked) GetById(id uint64) (domain.WorkflowInstance, error) {
	panic("implement me")
}

func (w *WorkflowRepositoryMocked) Save(tx pgx.Tx, instance *domain.WorkflowInstance) error {
	args := w.Called(tx, instance)
	return args.Error(0)
}

func (w *WorkflowRepositoryMocked) Update(tx pgx.Tx, instance domain.WorkflowInstance) error {
	panic("implement me")
}

type MessageQueueServiceMocked struct {
	mock.Mock
}

func (m *MessageQueueServiceMocked) Publish(stream string, streamName domain.StreamName, facts domain.Facts, opts ...liftbridge.MessageOption) error {
	panic("implement me")
}

func (m *MessageQueueServiceMocked) Subscribe(ctx context.Context, name domain.StreamName, handler liftbridge.Handler, i int32) error {
	panic("implement me")
}

func (m *MessageQueueServiceMocked) Save(tx pgx.Tx, message *liftbridge.Message) error {
	panic("implement me")
}

func buildTransactionMocked() (tx pgx.Tx, err error){
	mock, err := pgxmock.NewConn()
	if err != nil {
		err = errors.WithMessage(err, "an error '%s' was not expected when opening a stub database connection")
	}
	mock.ExpectBegin()
	tx, err = mock.Begin(context.Background())
	if err != nil {
		err = errors.WithMessage(err, "an error '%s' was not expected when Begin a Tx in database")
	}
	return
}

func buildWorkflowApplicationMocked(workflowRepositoryMocked *WorkflowRepositoryMocked, messageQueueServiceMocked *MessageQueueServiceMocked) *workflowService {
	return &workflowService{
		workflowRepository:  workflowRepositoryMocked,
		messageQueueService: messageQueueServiceMocked,
		logger: log.New(os.Stderr, "WorkflowApplicationTest: ", log.LstdFlags),
	}
}

func TestSavingWorkflowSuccess(t *testing.T) {
	t.Parallel()

	//given
	workflow := &domain.WorkflowInstance{
		ID: uint64(1),
		Name:   "Name",
		Source: "Source",
		Facts:  domain.Facts{},
	}
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	tx, err := buildTransactionMocked()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	workflowRepository.On("Save", tx, workflow).Return(nil)

	//when
	err = workflowService.Save(tx, workflow)

	//then
	assert.Equal(t, nil, err, "No error")
	messageQueueService.AssertExpectations(t)
}

func TestSavingWorkflowFailure(t *testing.T) {
	t.Parallel()

	//given
	workflow := &domain.WorkflowInstance{
		ID: uint64(1),
		Name:   "Name",
		Source: "Source",
		Facts:  domain.Facts{},
	}
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	tx, err := buildTransactionMocked()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	errorMessage := "Some error"
	workflowRepository.On("Save", tx, workflow).Return(errors.New(errorMessage))

	//when
	err = workflowService.Save(tx, workflow)

	//then
	assert.NotEqual(t, nil, err, "An error is thrown if connection to DB from repository fails")
	assert.Equal(t, err.Error(), fmt.Errorf("Couldn't insert workflow: %s", errorMessage).Error())
	messageQueueService.AssertExpectations(t)
}