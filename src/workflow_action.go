package cicero

import (
	"log"
	"os"

	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/pkg/errors"
)

type WorkflowActionService interface {
	GetWorkflowAction(model.ActionInstance) (model.WorkflowAction, error)
}

type WorkflowActionServiceImpl struct {
	logger          *log.Logger
	evaluator       *Evaluator
	workflowService service.WorkflowService
}

func NewWorkflowActionService(evaluator *Evaluator, workflowService service.WorkflowService) WorkflowActionService {
	return &WorkflowActionServiceImpl{
		logger:          log.New(os.Stderr, "WorkflowActionService: ", log.LstdFlags),
		evaluator:       evaluator,
		workflowService: workflowService,
	}
}

func (w *WorkflowActionServiceImpl) GetWorkflowAction(action model.ActionInstance) (def model.WorkflowAction, err error) {
	wf, err := w.workflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		err = errors.WithMessagef(err, "Could not get workflow instance for workflow instance with ID %d", action.WorkflowInstanceId)
		return
	}

	wfDef, err := w.evaluator.EvaluateWorkflow(wf.Source, wf.Name, wf.ID, wf.Facts)
	if err != nil {
		err = errors.WithMessagef(err, "Could not evaluate definition for workflow instance %#v", wf)
		return
	}

	def = *wfDef.Actions[action.Name]
	return
}
