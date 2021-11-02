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
	Name    string                  `json:"name"`
	Version uint64                  `json:"version"`
	Meta    map[string]interface{}  `json:"meta"`
	Steps   map[string]WorkflowStep `json:"steps"`
}

type WorkflowStep struct {
	Failure WorkflowCerts   `json:"failure"`
	Success WorkflowCerts   `json:"success"`
	Inputs  []string        `json:"inputs"`
	When    map[string]bool `json:"when"`
	Job     nomad.Job       `json:"job"`
}

func (s *WorkflowStep) IsRunnable() bool {
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
	var def WorkflowDefinition

	def, err := evaluator.EvaluateWorkflow(w.Name, w.ID, w.Certs)
	if err != nil {
		return def, err
	}

	return def, nil
}

type WorkflowCerts map[string]interface{}

type StepInstance struct {
	ID                 uuid.UUID
	WorkflowInstanceId uint64
	Name               string
	Certs              WorkflowCerts
	CreatedAt          *time.Time
	UpdatedAt          *time.Time
	FinishedAt         *time.Time
}

func (s *StepInstance) GetDefinition(logger *log.Logger, evaluator Evaluator) (WorkflowStep, error) {
	var def WorkflowStep

	wf, err := s.GetWorkflow()
	if err != nil {
		return def, err
	}

	wfDef, err := wf.GetDefinition(logger, evaluator)
	if err != nil {
		return def, err
	}

	return wfDef.Steps[s.Name], nil
}

func (s *StepInstance) GetWorkflow() (WorkflowInstance, error) {
	var instance WorkflowInstance

	err := pgxscan.Get(
		context.Background(), DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		s.WorkflowInstanceId,
	)
	if err != nil {
		return instance, errors.WithMessagef(err, "Could not get workflow instance for step %s", s.ID)
	}

	return instance, nil
}
