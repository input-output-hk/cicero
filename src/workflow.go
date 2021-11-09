package cicero

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/pkg/errors"
	"log"
)

//TODO: WIP move to service and repository

func GetDefinition(w model.WorkflowInstance, logger *log.Logger, evaluator Evaluator) (model.WorkflowDefinition, error) {
	return evaluator.EvaluateWorkflow(w.Name, &w.Version, w.ID, w.Certs)
}

func GetDefinitionByAction(actionInstance model.ActionInstance, logger *log.Logger, evaluator Evaluator) (def model.WorkflowAction, err error) {
	wf, err := GetWorkflow(actionInstance)
	if err != nil {
		return
	}

	wfDef, err := GetDefinition(wf, logger, evaluator)
	if err != nil {
		return
	}

	def = *wfDef.Actions[actionInstance.Name]
	return
}

func GetWorkflow(actionInstance model.ActionInstance) (instance model.WorkflowInstance, err error) {
	err = pgxscan.Get(
		context.Background(), DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		actionInstance.WorkflowInstanceId,
	)
	if err != nil {
		err = errors.WithMessagef(err, "Could not get workflow instance for action %s", actionInstance.ID)
	}
	return
}
