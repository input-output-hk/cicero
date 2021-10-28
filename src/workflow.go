package cicero

import (
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
	Name      string        `bun:",notnull"`
	Certs     WorkflowCerts `bun:",notnull"`
	CreatedAt time.Time     `bun:",nullzero,notnull,default:current_timestamp"`
	UpdatedAt time.Time     `bun:",nullzero,notnull,default:current_timestamp"`
}

type WorkflowCerts map[string]interface{}

type StepInstance struct {
	ID                 uuid.UUID     `bun:",notnull,type:blob"` // FIXME still stored as string
	WorkflowInstanceId uint64        `bun:",notnull"`
	Name               string        `bun:",notnull"`
	Certs              WorkflowCerts `bun:",notnull"`
	CreatedAt          time.Time     `bun:",nullzero,notnull,default:current_timestamp"`
	FinishedAt         time.Time     `bun:",nullzero"`
}
