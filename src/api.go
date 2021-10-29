package cicero

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/google/uuid"
	"github.com/liftbridge-io/go-liftbridge"
)

type Api struct {
	logger *log.Logger
	bridge liftbridge.Client
	evaluator Evaluator
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

		def, err = instance.GetDefinition(logger, a.evaluator)
		if err != nil {
			return def, err
		}
		return def, nil
	} else {
		var err error
		def, err := a.Workflow(wfName)
		if err != nil {
			return def, err
		}
		return def, nil
	}
}

// TODO superfluous?
func (a *Api) Workflows() ([]string, error) {
	return a.evaluator.ListWorkflows()
}

// TODO superfluous?
func (a *Api) Workflow(name string) (WorkflowDefinition, error) {
	return a.evaluator.EvaluateWorkflow(name, 0, WorkflowCerts{})
}

func (a *Api) WorkflowStart(name string) error {
	return publish(a.logger, a.bridge, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", WorkflowCerts{})
}

func (a *Api) Steps() ([]StepInstance, error) {
	instances := make([]StepInstance, 0)

	err := DB.NewSelect().
		Model(&instances).
		Scan(context.Background())
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) Step(id uuid.UUID) (StepInstance, error) {
	var instance StepInstance

	err := DB.NewSelect().
		Model(&instance).
		Where("id = ?", id).
		Scan(context.Background())
	if err != nil {
		return instance, err
	}

	return instance, nil
}
