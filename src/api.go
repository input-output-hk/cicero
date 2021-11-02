package cicero

import (
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"

	"github.com/liftbridge-io/go-liftbridge"
)

type Api struct {
	logger *log.Logger
	bridge liftbridge.Client
	workflowService service.WorkflowService
}

func (api *Api) init() {
	if api.logger == nil {
		api.logger = log.New(os.Stderr, "api: ", log.LstdFlags)
	}
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (model.WorkflowDefinition, error) {
	if instanceId != nil {
		var def model.WorkflowDefinition

		instance, err := a.workflowService.GetAllByNameAndId(wfName, *instanceId)
		if err != nil {
			return def, err
		}

		def, err = GetDefinition(instance, logger)
		if err != nil {
			return def, err
		}
		return def, nil
	} else {
		var err error
		def, err := a.Workflow(wfName, model.WorkflowCerts{})
		if err != nil {
			return def, err
		}
		return def, nil
	}
}

func (a *Api) Workflows() (model.WorkflowDefinitions, error) {
	return nixInstantiate(a.logger, "workflows", 0, "{}")
}

func (a *Api) Workflow(name string, certs model.WorkflowCerts) (model.WorkflowDefinition, error) {
	var def model.WorkflowDefinition
	certsJson, err := json.Marshal(certs)
	if err != nil {
		return def, err
	}
	return nixInstantiateWorkflow(a.logger, name, 0, string(certsJson))
}

func (a *Api) WorkflowStart(name string) error {
	return service.Publish(a.logger, a.bridge, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", model.WorkflowCerts{})
}
