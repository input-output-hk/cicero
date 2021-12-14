package application

import (
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
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
	logger              zerolog.Logger
	workflowRepository  repository.WorkflowRepository
	messageQueueService MessageQueueService
}

func NewWorkflowService(db config.PgxIface, messageQueueService MessageQueueService, logger *zerolog.Logger) WorkflowService {
	return &workflowService{
		logger:              logger.With().Str("component", "WorkflowService").Logger(),
		workflowRepository:  persistence.NewWorkflowRepository(db),
		messageQueueService: messageQueueService,
	}
}

func (s *workflowService) GetSummary() (domain.WorkflowSummary, error) {
	s.logger.Info().Msg("Get Summary")
	return s.workflowRepository.GetSummary()
}

func (s *workflowService) GetAll() ([]*domain.WorkflowInstance, error) {
	s.logger.Info().Msg("Get all Workflows")
	return s.workflowRepository.GetAll()
}

func (s *workflowService) GetAllByName(name string) ([]*domain.WorkflowInstance, error) {
	s.logger.Info().Msgf("Get all Workflows by name %s", name)
	return s.workflowRepository.GetAllByName(name)
}

func (s *workflowService) GetById(id uint64) (workflow domain.WorkflowInstance, err error) {
	s.logger.Info().Msgf("Get Workflow by id %d", id)
	workflow, err = s.workflowRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing workflow for id %d", id)
	return
}

func (s *workflowService) Save(tx pgx.Tx, workflow *domain.WorkflowInstance) error {
	s.logger.Info().Msgf("Saving new Workflow %s", workflow.Name)
	if err := s.workflowRepository.Save(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not insert workflow")
	}
	s.logger.Info().Msgf("Created workflow %d", workflow.ID)
	return nil
}

func (s *workflowService) Update(tx pgx.Tx, workflow domain.WorkflowInstance) error {
	s.logger.Info().Msgf("Update workflow %d", workflow.ID)
	if err := s.workflowRepository.Update(tx, workflow); err != nil {
		return errors.WithMessage(err, "Could not update workflow")
	}
	s.logger.Info().Msgf("Updated workflow %d", workflow.ID)
	return nil
}

func (s *workflowService) Start(source, name string, inputs domain.Facts) error {
	return s.messageQueueService.Publish(
		domain.StartStreamName.Fmt(name),
		domain.StartStreamName,
		inputs,
		s.messageQueueService.BuildMessage("source", []byte(source)),
	)
}
