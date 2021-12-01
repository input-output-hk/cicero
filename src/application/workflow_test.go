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

func (w *WorkflowRepositoryMocked) GetById(id uint64) (workflow domain.WorkflowInstance, err error) {
	args := w.Called(id)
	if args.Get(0) != nil {
		workflow = args.Get(0).(domain.WorkflowInstance)
	}
	err = args.Error(1)
	return
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

func (m *MessageQueueServiceMocked) BuildMessage(name string, value []byte) liftbridge.MessageOption {
	args := m.Called(name, value)
	return args.Get(0).(liftbridge.MessageOption)
}

func (m *MessageQueueServiceMocked) Publish(stream string, streamName domain.StreamName, facts domain.Facts, opts ...liftbridge.MessageOption) error {
	args := m.Called(stream, streamName, facts, opts)
	return args.Error(0)
}

func (m *MessageQueueServiceMocked) Subscribe(ctx context.Context, name domain.StreamName, handler liftbridge.Handler, i int32) error {
	panic("implement me")
}

func (m *MessageQueueServiceMocked) Save(tx pgx.Tx, message *liftbridge.Message) error {
	panic("implement me")
}

func buildTransactionMocked() (tx pgx.Tx, err error) {
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
		logger:              log.New(os.Stderr, "WorkflowApplicationTest: ", log.LstdFlags),
	}
}

func TestSavingWorkflowSuccess(t *testing.T) {
	t.Parallel()

	//given
	workflow := &domain.WorkflowInstance{ID: uint64(1)}
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
	workflowRepository.AssertExpectations(t)
}

func TestSavingWorkflowFailure(t *testing.T) {
	t.Parallel()

	//given
	workflow := &domain.WorkflowInstance{ID: uint64(1)}
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
	assert.Equal(t, err.Error(), fmt.Errorf("Couldn't insert workflow: %s", errorMessage).Error())
	workflowRepository.AssertExpectations(t)
}

func TestGettingWorkflowByIdSuccess(t *testing.T) {
	t.Parallel()

	//given
	workflowId := uint64(1)
	workflow := domain.WorkflowInstance{ID: uint64(1)}
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	workflowRepository.On("GetById", workflowId).Return(workflow, nil)

	//when
	workflowResult, err := workflowService.GetById(workflowId)

	//then
	assert.Equal(t, nil, err, "No error")
	assert.Equal(t, workflowResult, workflow)
	workflowRepository.AssertExpectations(t)
}

func TestGettingWorkflowByIdFailure(t *testing.T) {
	t.Parallel()

	//given
	workflowId := uint64(1)
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	errorMessage := "Some error"
	workflowRepository.On("GetById", workflowId).Return(nil, errors.New(errorMessage))

	//when
	_, err := workflowService.GetById(workflowId)

	//then
	assert.Equal(t, err.Error(), fmt.Errorf("Couldn't select existing workflow for id %d: %s", workflowId, errorMessage).Error())
	workflowRepository.AssertExpectations(t)
}

func TestStartWorkflowSuccess(t *testing.T) {
	t.Parallel()

	//given
	source := "source"
	name := "name"
	inputs := domain.Facts{}
	sourceBytes := []byte(source)
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	messageQueueService.On("BuildMessage", source, sourceBytes).Return(liftbridge.Header(source, sourceBytes))
	messageQueueService.On("Publish",
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		mock.AnythingOfType("[]liftbridge.MessageOption")).Return(nil)

	//when
	err := workflowService.Start(source, name, inputs)

	//then
	assert.Equal(t, nil, err, "No error")
	messageQueueService.AssertExpectations(t)
}

func TestStartWorkflowFailure(t *testing.T) {
	t.Parallel()

	//given
	source := "source"
	name := "name"
	inputs := domain.Facts{}
	sourceBytes := []byte(source)
	workflowRepository := new(WorkflowRepositoryMocked)
	messageQueueService := new(MessageQueueServiceMocked)
	workflowService := buildWorkflowApplicationMocked(workflowRepository, messageQueueService)
	errorMessage := "Some Error"
	messageQueueService.On("BuildMessage", source, sourceBytes).Return(liftbridge.Header(source, sourceBytes))
	messageQueueService.On("Publish",
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		mock.AnythingOfType("[]liftbridge.MessageOption")).Return(errors.New(errorMessage))

	//when
	err := workflowService.Start(source, name, inputs)

	//then
	assert.Equal(t, errorMessage, err.Error())
	messageQueueService.AssertExpectations(t)
}
