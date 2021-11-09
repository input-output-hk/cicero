package cicero

import (
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"
)

type WorkflowActionService interface {
	GetWorkflowAction(*model.ActionInstance) (*model.WorkflowAction, error)
}

type WorkflowActionServiceImpl struct {
	logger          *log.Logger
	evaluator       Evaluator
	workflowService service.WorkflowService
}

func NewWorkflowActionService(evaluator Evaluator, workflowService service.WorkflowService) WorkflowActionService {
	return &WorkflowActionServiceImpl{
		logger: log.New(os.Stderr, "WorkflowActionService: ", log.LstdFlags),
		evaluator: evaluator,
		workflowService: workflowService,
	}
}

func (w *WorkflowActionServiceImpl) GetWorkflowAction(action *model.ActionInstance) (*model.WorkflowAction, error) {
	wf, err := w.workflowService.GetById(action.WorkflowInstanceId)
	if err != nil {
		log.Printf("Could not get workflow instance for WorkflowInstanceId %d", action.WorkflowInstanceId)
		return nil, err
	}

	wfDef, err := w.evaluator.EvaluateWorkflow(wf.Name, &wf.Version, wf.ID, wf.Certs)
	if err != nil {
		log.Printf("Could Evaluate Workflow for Workflow %#v", wf)
		return nil, err
	}

	return wfDef.Actions[action.Name], nil
}
