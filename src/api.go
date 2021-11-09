package cicero

import (
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"
)

type Api struct {
	logger          *log.Logger
	workflowService service.WorkflowService
	evaluator       Evaluator
}

func (api *Api) init() {
	if api.logger == nil {
		api.logger = log.New(os.Stderr, "api: ", log.LstdFlags)
	}
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (def model.WorkflowDefinition, instance *model.WorkflowInstance, err error) {
	if instanceId != nil {
		var inst model.WorkflowInstance
		if inst, err = a.workflowService.GetById(*instanceId); err != nil {
			return
		} else {
			instance = &inst
		}

		def, err = GetDefinition(*instance, logger, a.evaluator)
		return
	} else {
		def, err = a.Workflow(wfName, nil)
		return
	}
}

// TODO superfluous?
func (a *Api) Workflows() ([]string, error) {
	return a.evaluator.ListWorkflows(nil)
}

// TODO superfluous?
func (a *Api) Workflow(name string, version *string) (model.WorkflowDefinition, error) {
	return a.evaluator.EvaluateWorkflow(name, version, 0, model.WorkflowCerts{})
}
