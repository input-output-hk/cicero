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
	return nixInstantiate(a.logger, "workflows", 0, WorkflowCerts{})
}

func (a *Api) Workflow(name string, certs WorkflowCerts) (WorkflowDefinition, error) {
	return nixInstantiateWorkflow(a.logger, name, 0, certs)
}

func (a *Api) WorkflowStart(name string) error {
	return publish(a.logger, a.bridge, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", WorkflowCerts{})
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
