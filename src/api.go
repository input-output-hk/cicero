package cicero

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

type Api struct {
	logger *log.Logger
}

func (api *Api) init() {
	if api.logger == nil {
		api.logger = log.New(os.Stderr, "api: ", log.LstdFlags)
	}
}

func (a *Api) WorkflowInstances(name string) ([]WorkflowInstance, error) {
	instances := []WorkflowInstance{}

	err := DB.NewSelect().
		Model(&instances).
		Where("name = ?", name).
		Scan(context.Background())
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) WorkflowInstance(wfName string, id uint64) (WorkflowInstance, error) {
	var instance WorkflowInstance

	err := DB.NewSelect().
		Model(&instance).
		Where("name = ? AND id = ?", wfName, id).
		Scan(context.Background())
	if err != nil {
		return instance, err
	}

	return instance, nil
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (WorkflowDefinition, error) {
	if instanceId != nil {
		var def WorkflowDefinition

		instance, err := a.WorkflowInstance(wfName, *instanceId)
		if err != nil {
			return def, err
		}

		def, err = instance.GetDefinition(logger)
		if err != nil {
			return def, err
		}
		return def, nil
	} else {
		var err error
		def, err := a.Workflow(wfName, WorkflowCerts{})
		if err != nil {
			return def, err
		}
		return def, nil
	}
}

func (a *Api) Workflows() (WorkflowDefinitions, error) {
	return nixInstantiate(a.logger, "workflows", 0, "{}")
}

func (a *Api) Workflow(name string, certs WorkflowCerts) (WorkflowDefinition, error) {
	var def WorkflowDefinition
	certsJson, err := json.Marshal(certs)
	if err != nil {
		return def, err
	}
	return nixInstantiateWorkflow(a.logger, name, 0, string(certsJson))
}

func (a *Api) WorkflowStart(name string) error {
	return publish(a.logger, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", WorkflowCerts{})
}
