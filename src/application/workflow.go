package application

import (
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"log"
	"os"

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

type workflowService struct {
	logger              *log.Logger
	workflowRepository  repository.WorkflowRepository
	messageQueueService MessageQueueService
}

func NewWorkflowService(db *pgxpool.Pool, messageQueueService MessageQueueService) WorkflowService {
	return &workflowService{
		logger:              log.New(os.Stderr, "WorkflowApplication: ", log.LstdFlags),
		workflowRepository:  persistence.NewWorkflowRepository(db),
		messageQueueService: messageQueueService,
	}
}

func (s *workflowService) GetSummary() (domain.WorkflowSummary, error) {
	s.logger.Println("Get Summary")
	return s.workflowRepository.GetSummary()
}

func (s *workflowService) GetAll() ([]*domain.WorkflowInstance, error) {
	s.logger.Println("Get all Workflows")
	return s.workflowRepository.GetAll()
}

func (s *workflowService) GetAllByName(name string) ([]*domain.WorkflowInstance, error) {
	s.logger.Printf("Get all Workflows by name %s", name)
	return s.workflowRepository.GetAllByName(name)
}

func (s *workflowService) GetById(id uint64) (workflow domain.WorkflowInstance, err error) {
	s.logger.Printf("Get Workflow by id %d", id)
	workflow, err = s.workflowRepository.GetById(id)
	err = errors.WithMessagef(err, "Couldn't select existing workflow for id %d", id)
	return
}

func (s *workflowService) Save(tx pgx.Tx, workflow *domain.WorkflowInstance) error {
	s.logger.Printf("Saving new Workflow %#v", workflow)
	if err := s.workflowRepository.Save(tx, workflow); err != nil {
		return errors.WithMessagef(err, "Couldn't insert workflow")
	}
	s.logger.Printf("Created workflow %#v", workflow)
	return nil
}

func (s *workflowService) Update(tx pgx.Tx, workflow domain.WorkflowInstance) error {
	s.logger.Printf("Update workflow %#v", workflow)
	if err := s.workflowRepository.Update(tx, workflow); err != nil {
		return errors.WithMessage(err, "Couldn't update workflow")
	}
	s.logger.Printf("Updated workflow %#v", workflow)
	return nil
}

func (s *workflowService) Start(source string, name string, inputs domain.Facts) error {
	return s.messageQueueService.Publish(
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		liftbridge.Header("source", []byte(source)),
	)
}
