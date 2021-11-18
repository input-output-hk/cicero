package service

import (
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
	GetSummary() (repository.WorkflowSummary, error)
	GetAll() ([]*model.WorkflowInstance, error)
	GetAllByName(string) ([]*model.WorkflowInstance, error)
	GetById(uint64) (model.WorkflowInstance, error)
	Save(pgx.Tx, *model.WorkflowInstance) error
	Update(pgx.Tx, model.WorkflowInstance) error
	Start(string, string, model.Facts) error
}

type workflowService struct {
	logger              *log.Logger
	workflowRepository  repository.WorkflowRepository
	messageQueueService MessageQueueService
}

func NewWorkflowService(db *pgxpool.Pool, messageQueueService MessageQueueService) WorkflowService {
	return &workflowService{
		logger:              log.New(os.Stderr, "WorkflowService: ", log.LstdFlags),
		workflowRepository:  repository.NewWorkflowRepository(db),
		messageQueueService: messageQueueService,
	}
}

func (s *workflowService) GetSummary() (repository.WorkflowSummary, error) {
	log.Println("Get Summary")
	return s.workflowRepository.GetSummary()
}

func (s *workflowService) GetAll() ([]*model.WorkflowInstance, error) {
	log.Println("Get all Workflows")
	return s.workflowRepository.GetAll()
}

func (s *workflowService) GetAllByName(name string) ([]*model.WorkflowInstance, error) {
	log.Printf("Get all Workflows by name %s", name)
	return s.workflowRepository.GetAllByName(name)
}

func (s *workflowService) GetById(id uint64) (workflow model.WorkflowInstance, err error) {
	log.Printf("Get Workflow by id %d", id)
	workflow, err = s.workflowRepository.GetById(id)
	err = errors.WithMessagef(err, "Couldn't select existing workflow for id %d", id)
	return
}

func (s *workflowService) Save(tx pgx.Tx, workflow *model.WorkflowInstance) error {
	log.Printf("Saving new Workflow %#v", workflow)
	if err := s.workflowRepository.Save(tx, workflow); err != nil {
		return errors.WithMessagef(err, "Couldn't insert workflow")
	}
	log.Printf("Created workflow %#v", workflow)
	return nil
}

func (s *workflowService) Update(tx pgx.Tx, workflow model.WorkflowInstance) error {
	log.Printf("Update workflow %#v", workflow)
	if err := s.workflowRepository.Update(tx, workflow); err != nil {
		return errors.WithMessage(err, "Couldn't update workflow")
	}
	log.Printf("Updated workflow %#v", workflow)
	return nil
}

func (s *workflowService) Start(source, name string, inputs model.Facts) error {
	return s.messageQueueService.Publish(
		model.StartStreamName.Fmt(name),
		model.StartStreamName,
		inputs,
		liftbridge.Header("source", []byte(source)),
	)
}
