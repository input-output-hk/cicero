package cicero

import (
	"context"
	"log"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/pkg/errors"
)

type WorkflowDefinitions map[string]*WorkflowDefinition

type WorkflowDefinition struct {
	Name    string                     `json:"name"`
	Version uint64                     `json:"version"`
	Meta    map[string]interface{}     `json:"meta"`
	Actions map[string]*WorkflowAction `json:"actions"`
}

type WorkflowAction struct {
	Failure WorkflowCerts   `json:"failure"`
	Success WorkflowCerts   `json:"success"`
	Inputs  []string        `json:"inputs"`
	When    map[string]bool `json:"when"`
	Job     nomad.Job       `json:"job"`
}

func (s *WorkflowAction) IsRunnable() bool {
	return len(s.Job.TaskGroups) > 0
}

type WorkflowInstance struct {
	ID        uint64
	Name      string
	Certs     WorkflowCerts
	CreatedAt *time.Time
	UpdatedAt *time.Time
}

func (w *WorkflowInstance) GetDefinition(logger *log.Logger, evaluator Evaluator) (WorkflowDefinition, error) {
	return evaluator.EvaluateWorkflow(w.Name, w.ID, w.Certs)
}

type WorkflowCerts map[string]interface{}

type ActionInstance struct {
	ID                 uuid.UUID
	WorkflowInstanceId uint64
	Name               string
	Certs              WorkflowCerts
	CreatedAt          *time.Time
	UpdatedAt          *time.Time
	FinishedAt         *time.Time
}

func (s *ActionInstance) GetDefinition(logger *log.Logger, evaluator Evaluator) (*WorkflowAction, error) {
	wf, err := s.GetWorkflow()
	if err != nil {
		return nil, err
	}

	wfDef, err := wf.GetDefinition(logger, evaluator)
	if err != nil {
		return nil, err
	}

	return wfDef.Actions[s.Name], nil
}

func (s *ActionInstance) GetWorkflow() (WorkflowInstance, error) {
	var instance WorkflowInstance

	if err := pgxscan.Get(
		context.Background(), DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		s.WorkflowInstanceId,
	); err != nil {
		return instance, errors.WithMessagef(err, "Could not get workflow instance for action %s", s.ID)
	}

	return instance, nil
}
