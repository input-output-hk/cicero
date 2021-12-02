package application

import (
	"github.com/input-output-hk/cicero/src/domain"
	"log"
	"os"

	"github.com/pkg/errors"
)

type WorkflowActionService interface {
	GetWorkflowAction(domain.ActionInstance) (domain.WorkflowAction, error)
}

type workflowActionService struct {
	logger            *log.Logger
	evaluationService EvaluationService
	workflowService   WorkflowService
}

func NewWorkflowActionService(evaluationService EvaluationService, workflowService WorkflowService) WorkflowActionService {
	return &workflowActionService{
		logger:            log.New(os.Stderr, "WorkflowActionService: ", log.LstdFlags),
		evaluationService: evaluationService,
		workflowService:   workflowService,
	}
}

func (w *workflowActionService) GetWorkflowAction(action domain.ActionInstance) (def domain.WorkflowAction, err error) {
	wf, err := w.workflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		err = errors.WithMessagef(err, "Could not get workflow instance for workflow instance with ID %d", action.WorkflowInstanceId)
		return
	}

	wfDef, err := w.evaluationService.EvaluateWorkflow(wf.Source, wf.Name, wf.ID, wf.Facts)
	if err != nil {
		err = errors.WithMessagef(err, "Could not evaluate definition for workflow instance %d", wf.ID)
		return
	}

	def = *wfDef.Actions[action.Name]
	return
}
