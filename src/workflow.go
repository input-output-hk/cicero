package cicero

import (
	"log"
	"time"

	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
)

type WorkflowDefinitions map[string]*WorkflowDefinition

type WorkflowDefinition struct {
	Name    string                  `json:"name"`
	Version uint64                  `json:"version"`
	Meta    map[string]interface{}  `json:"meta"`
	Steps   map[string]WorkflowStep `json:"steps"`
}

type WorkflowStep struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Job     nomad.Job              `json:"job"`
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
	FinishedAt         *time.Time
}
