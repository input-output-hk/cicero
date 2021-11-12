package service

import (
	"fmt"
	"log"
	"os"

	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowService interface {
	GetAll() ([]*model.WorkflowInstance, error)
	GetAllByName(string) ([]*model.WorkflowInstance, error)
	GetById(uint64) (model.WorkflowInstance, error)
	Save(pgx.Tx, *model.WorkflowInstance) error
	Update(pgx.Tx, uint64, model.WorkflowInstance) error
	Start(string, string) error
}

type WorkflowServiceImpl struct {
	logger              *log.Logger
	workflowRepository  repository.WorkflowRepository
	messageQueueService *MessageQueueService
}

func NewWorkflowService(db *pgxpool.Pool, messageQueueService *MessageQueueService) WorkflowService {
	return &WorkflowServiceImpl{
		logger:              log.New(os.Stderr, "WorkflowService: ", log.LstdFlags),
		workflowRepository:  repository.NewWorkflowRepository(db),
		messageQueueService: messageQueueService,
	}
}

func (s *WorkflowServiceImpl) GetAll() ([]*model.WorkflowInstance, error) {
	log.Printf("Get all Workflows")
	return s.workflowRepository.GetAll()
}

func (s *WorkflowServiceImpl) GetAllByName(name string) ([]*model.WorkflowInstance, error) {
	log.Printf("Get all Workflows by name %s", name)
	return s.workflowRepository.GetAllByName(name)
}

func (s *WorkflowServiceImpl) GetById(id uint64) (workflow model.WorkflowInstance, err error) {
	log.Printf("Get Workflow by id %d", id)
	workflow, err = s.workflowRepository.GetById(id)
	err = errors.WithMessagef(err, "Couldn't select existing workflow for id %d", id)
	return
}

func (s *WorkflowServiceImpl) Save(tx pgx.Tx, workflow *model.WorkflowInstance) error {
	log.Printf("Saving new Workflow %#v", workflow)
	if err := s.workflowRepository.Save(tx, workflow); err != nil {
		return errors.WithMessagef(err, "Couldn't insert workflow")
	}
	log.Printf("Created workflow %#v", workflow)
	return nil
}

func (s *WorkflowServiceImpl) Update(tx pgx.Tx, id uint64, workflow model.WorkflowInstance) error {
	log.Printf("Update workflow %#v with id %d", workflow, id)
	if err := s.workflowRepository.Update(tx, id, workflow); err != nil {
		return errors.WithMessagef(err, "Couldn't update workflow with id: %d, error: %s", id)
	}
	log.Printf("Updated workflow %#v with id %d", workflow, id)
	return nil
}

func (s *WorkflowServiceImpl) Start(source string, name string) error {

	return (*s.messageQueueService).Publish(
		fmt.Sprintf("workflow.%s.start", name),
		StartStreamName,
		model.WorkflowCerts{},
		liftbridge.Header("source", []byte(source)),
	)
}
