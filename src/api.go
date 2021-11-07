package cicero

import (
	"context"
	"fmt"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/liftbridge-io/go-liftbridge"
)

type Api struct {
	logger    *log.Logger
	bridge    liftbridge.Client
	workflowService service.WorkflowService
	evaluator Evaluator
}

func (api *Api) init() {
	if api.logger == nil {
		api.logger = log.New(os.Stderr, "api: ", log.LstdFlags)
	}
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (model.WorkflowDefinition, error) {
	if instanceId != nil {
		var def model.WorkflowDefinition
		instance, err := a.workflowService.GetById(*instanceId)

		if err != nil {
			return def, err
		}

		def, err = GetDefinition(&instance, logger, a.evaluator)
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
func (a *Api) Workflow(name string) (model.WorkflowDefinition, error) {
	return a.evaluator.EvaluateWorkflow(name, 0, model.WorkflowCerts{})
}

func (a *Api) WorkflowStart(name string) error {
	return service.Publish(a.logger, a.bridge, fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", model.WorkflowCerts{})
}

func (a *Api) Actions() ([]*model.ActionInstance, error) {
	instances := []*model.ActionInstance{}

	err := pgxscan.Select(
		context.Background(),
		DB,
		&instances,
		`SELECT * FROM action_instances ORDER BY id DESC`)
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) Action(id uuid.UUID) (*model.ActionInstance, error) {
	instance := &model.ActionInstance{}

	err := pgxscan.Get(
		context.Background(), DB, instance,
		`SELECT * FROM action_instances WHERE id = $1`,
		id,
	)
	if err != nil {
		return nil, err
	}

	return instance, nil
}
