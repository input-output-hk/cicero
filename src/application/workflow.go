package application

import (
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"log"
	"os"

	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

type WorkflowService interface {
	GetSummary() (domain.WorkflowSummary, error)
	GetAll() ([]*domain.WorkflowInstance, error)
	GetAllByName(string) ([]*domain.WorkflowInstance, error)
	GetById(uint64) (domain.WorkflowInstance, error)
	Save(pgx.Tx, *domain.WorkflowInstance) error
	Update(pgx.Tx, domain.WorkflowInstance) error
	Start(string, string, domain.Facts) error
}

type WorkflowServiceImpl struct {
	logger              *log.Logger
	workflowRepository  repository.WorkflowRepository
	messageQueueService MessageQueueService
}

func NewWorkflowService(db *pgxpool.Pool, messageQueueService MessageQueueService) WorkflowService {
	return &WorkflowServiceImpl{
		logger:              log.New(os.Stderr, "WorkflowService: ", log.LstdFlags),
		workflowRepository:  persistence.NewWorkflowRepository(db),
		messageQueueService: messageQueueService,
	}
}

func (s *WorkflowServiceImpl) GetSummary() (domain.WorkflowSummary, error) {
	log.Println("Get Summary")
	return s.workflowRepository.GetSummary()
}

func (s *WorkflowServiceImpl) GetAll() ([]*domain.WorkflowInstance, error) {
	log.Println("Get all Workflows")
	return s.workflowRepository.GetAll()
}

func (s *WorkflowServiceImpl) GetAllByName(name string) ([]*domain.WorkflowInstance, error) {
	log.Printf("Get all Workflows by name %s", name)
	return s.workflowRepository.GetAllByName(name)
}

func (s *WorkflowServiceImpl) GetById(id uint64) (workflow domain.WorkflowInstance, err error) {
	log.Printf("Get Workflow by id %d", id)
	workflow, err = s.workflowRepository.GetById(id)
	err = errors.WithMessagef(err, "Couldn't select existing workflow for id %d", id)
	return
}

func (s *WorkflowServiceImpl) Save(tx pgx.Tx, workflow *domain.WorkflowInstance) error {
	log.Printf("Saving new Workflow %#v", workflow)
	if err := s.workflowRepository.Save(tx, workflow); err != nil {
		return errors.WithMessagef(err, "Couldn't insert workflow")
	}
	log.Printf("Created workflow %#v", workflow)
	return nil
}

func (s *WorkflowServiceImpl) Update(tx pgx.Tx, workflow domain.WorkflowInstance) error {
	log.Printf("Update workflow %#v", workflow)
	if err := s.workflowRepository.Update(tx, workflow); err != nil {
		return errors.WithMessage(err, "Couldn't update workflow")
	}
	log.Printf("Updated workflow %#v", workflow)
	return nil
}

func (s *WorkflowServiceImpl) Start(source, name string, inputs domain.Facts) error {
	return s.messageQueueService.Publish(
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		liftbridge.Header("source", []byte(source)),
	)
}
