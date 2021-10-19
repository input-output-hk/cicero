package cicero

import (
	"context"
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

func (a *Api) Workflow(name string) (*workflowDefinition, error) {
	return nixInstantiateWorkflow(a.logger, name, 0, "{}")
}

func (a *Api) WorkflowInstances(name string) ([]Workflow, error) {
	instances := []Workflow{}

	err := db.NewSelect().
		Model(&instances).
		Where("name = ?", name).
		Scan(context.Background())
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) Workflows() (*workflowDefinitions, error) {
	return nixInstantiate(a.logger, "workflows", 0, "{}")
}

func (a *Api) WorkflowStart(name string) error {
	publish(a.logger, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", map[string]interface{}{})
	return nil
}
