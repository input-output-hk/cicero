package cicero

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/pkg/errors"
	"log"
)

//TODO: WIP move to service and repository

func GetDefinition(w *model.WorkflowInstance, logger *log.Logger, evaluator Evaluator) (model.WorkflowDefinition, error) {
	return evaluator.EvaluateWorkflow(w.Name, &w.Version, w.ID, w.Certs)
}

func GetDefinitionByAction(s *model.ActionInstance, logger *log.Logger, evaluator Evaluator) (*model.WorkflowAction, error) {
	wf, err := GetWorkflow(s)
	if err != nil {
		return nil, err
	}

	wfDef, err := GetDefinition(&wf, logger, evaluator)
	if err != nil {
		return nil, err
	}

	return wfDef.Actions[s.Name], nil
}

func GetWorkflow(s *model.ActionInstance) (model.WorkflowInstance, error) {
	var instance model.WorkflowInstance

	if err := pgxscan.Get(
		context.Background(), DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		s.WorkflowInstanceId,
	); err != nil {
		return instance, errors.WithMessagef(err, "Could not get workflow instance for action %s", s.ID)
	}

	return instance, nil
}
