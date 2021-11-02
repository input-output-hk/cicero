package service

import (
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	log "github.com/sirupsen/logrus"
	"github.com/uptrace/bun"
)

type WorkflowService interface {
	GetAllByName(string)([]model.WorkflowInstance, error)
	GetAllByNameAndId(string, uint64)(model.WorkflowInstance, error)
	WithTrx(*bun.DB) workflowService
}

type workflowService struct {
	workflowRepository repository.WorkflowRepository
}

func NewWorkflowService(repository repository.WorkflowRepository) WorkflowService {
	return workflowService{workflowRepository: repository}
}

func (w workflowService) GetAllByName(name string) ([]model.WorkflowInstance, error) {
	log.Debug("[WorkflowService]...get all Workflows by name %s", name)
	return w.workflowRepository.GetAllByName(name)
}

func (w workflowService) GetAllByNameAndId(name string, id uint64) (model.WorkflowInstance, error) {
	log.Debug("[WorkflowService]...get all Workflows by name %s and id %d", name, id)
	return w.workflowRepository.GetAllByNameAndId(name, id)
}

// WithTrx enables repository with transaction
func (w workflowService) WithTrx(trxHandle *bun.DB) workflowService {
	w.workflowRepository = w.workflowRepository.WithTrx(trxHandle)
	return w
}

