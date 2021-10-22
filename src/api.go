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

func (a *Api) WorkflowInstances(name string) ([]Workflow, error) {
	instances := []Workflow{}

	err := DB.NewSelect().
		Model(&instances).
		Where("name = ?", name).
		Scan(context.Background())
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) WorkflowInstance(wfName string, id uint64) (Workflow, error) {
	var instance Workflow

	err := DB.NewSelect().
		Model(&instance).
		Where("name = ? AND id = ?", wfName, id).
		Scan(context.Background())
	if err != nil {
		return instance, err
	}

	return instance, nil
}

func (a *Api) Workflows() (*workflowDefinitions, error) {
	return nixInstantiate(a.logger, "workflows", 0, "{}")
}

func (a *Api) Workflow(name string, certs WorkflowCerts) (*workflowDefinition, error) {
	certsJson, err := json.Marshal(certs)
	if err != nil {
		return nil, err
	}
	return nixInstantiateWorkflow(a.logger, name, 0, string(certsJson))
}

func (a *Api) WorkflowStart(name string) error {
	publish(a.logger, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", WorkflowCerts{})
	return nil
}
