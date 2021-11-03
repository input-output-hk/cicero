package cicero

import (
	"encoding/json"
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

func (a *Api) WorkflowInstances(name string) ([]*WorkflowInstance, error) {
	instances := []*WorkflowInstance{}

	err := pgxscan.Select(
		context.Background(),
		DB,
		&instances,
		`SELECT * FROM workflow_instances WHERE name = $1`,
		name)
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (WorkflowDefinition, error) {
	if instanceId != nil {
		var def model.WorkflowDefinition

		instance := &WorkflowInstance{}
		err := pgxscan.Get(
			context.Background(), DB, instance,
			`SELECT * FROM workflow_instances WHERE id = $1`,
			*instanceId,
		)
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

func (a *Api) Actions() ([]*ActionInstance, error) {
	instances := []*ActionInstance{}

	err := pgxscan.Select(
		context.Background(),
		DB,
		&instances,
		`SELECT * FROM action_instances`)
	if err != nil {
		return nil, err
	}

	return instances, nil
}

func (a *Api) Action(id uuid.UUID) (*ActionInstance, error) {
	instance := &ActionInstance{}

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
