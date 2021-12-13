package application

import (
	"context"
	"fmt"
	"github.com/rs/zerolog/log"
	"testing"

	"github.com/input-output-hk/cicero/src/application/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	repositoryMocks "github.com/input-output-hk/cicero/src/domain/repository/mocks"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pashagolub/pgxmock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func buildTransactionMocked() (tx pgx.Tx, err error) {
	dbMock, err := pgxmock.NewConn()
	if err != nil {
		err = errors.WithMessage(err, "an error was not expected when opening a stub database connection")
		return
	}
	dbMock.ExpectBegin()
	tx, err = dbMock.Begin(context.Background())
	if err != nil {
		err = errors.WithMessage(err, "an error was not expected when beginning a DB transaction")
	}
	return
}

func buildWorkflowApplication(workflowRepositoryMocked *repositoryMocks.WorkflowRepository, messageQueueServiceMocked *mocks.MessageQueueService) *workflowService {
	return &workflowService{
		workflowRepository:  workflowRepositoryMocked,
		messageQueueService: messageQueueServiceMocked,
		logger:              log.Logger,
	}
}

func TestSavingWorkflowSuccess(t *testing.T) {
	t.Parallel()

	// given
	workflow := &domain.WorkflowInstance{ID: uint64(1)}
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	tx, err := buildTransactionMocked()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when beginning a DB transaction", err)
	}
	workflowRepository.On("Save", tx, workflow).Return(nil)

	// when
	err = workflowService.Save(tx, workflow)

	// then
	assert.Equal(t, nil, err, "No error")
	workflowRepository.AssertExpectations(t)
}

func TestSavingWorkflowFailure(t *testing.T) {
	t.Parallel()

	// given
	workflow := &domain.WorkflowInstance{ID: uint64(1)}
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	tx, err := buildTransactionMocked()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when beginning a DB transaction", err)
	}
	errorMessage := "Some error"
	workflowRepository.On("Save", tx, workflow).Return(errors.New(errorMessage))

	// when
	err = workflowService.Save(tx, workflow)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not insert workflow: %s", errorMessage).Error())
	workflowRepository.AssertExpectations(t)
}

func TestGettingWorkflowByIdSuccess(t *testing.T) {
	t.Parallel()

	// given
	workflowId := uint64(1)
	workflow := domain.WorkflowInstance{ID: uint64(1)}
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	workflowRepository.On("GetById", workflowId).Return(workflow, nil)

	// when
	workflowResult, err := workflowService.GetById(workflowId)

	// then
	assert.Equal(t, nil, err, "No error")
	assert.Equal(t, workflowResult, workflow)
	workflowRepository.AssertExpectations(t)
}

func TestGettingWorkflowByIdFailure(t *testing.T) {
	t.Parallel()

	// given
	workflowId := uint64(1)
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	errorMessage := "Some error"
	workflowRepository.On("GetById", workflowId).Return(domain.WorkflowInstance{}, errors.New(errorMessage))

	// when
	_, err := workflowService.GetById(workflowId)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not select existing workflow for id %d: %s", workflowId, errorMessage).Error())
	workflowRepository.AssertExpectations(t)
}

func TestStartWorkflowSuccess(t *testing.T) {
	t.Parallel()

	// given
	source := "source"
	name := "name"
	inputs := domain.Facts{}
	sourceBytes := []byte(source)
	message := liftbridge.Header(source, sourceBytes)
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	messageQueueService.On("BuildMessage", source, sourceBytes).Return(message)
	messageQueueService.On("Publish",
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		mock.AnythingOfType("liftbridge.MessageOption")).Return(nil)

	// when
	err := workflowService.Start(source, name, inputs)

	// then
	assert.Equal(t, nil, err, "No error")
	messageQueueService.AssertExpectations(t)
}

func TestStartWorkflowFailure(t *testing.T) {
	t.Parallel()

	// given
	source := "source"
	name := "name"
	inputs := domain.Facts{}
	sourceBytes := []byte(source)
	workflowRepository := &repositoryMocks.WorkflowRepository{}
	messageQueueService := &mocks.MessageQueueService{}
	workflowService := buildWorkflowApplication(workflowRepository, messageQueueService)
	errorMessage := "Some Error"
	messageQueueService.On("BuildMessage", source, sourceBytes).Return(liftbridge.Header(source, sourceBytes))
	messageQueueService.On("Publish",
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		mock.AnythingOfType("liftbridge.MessageOption")).Return(errors.New(errorMessage))

	// when
	err := workflowService.Start(source, name, inputs)

	// then
	assert.Equal(t, errorMessage, err.Error())
	messageQueueService.AssertExpectations(t)
}
