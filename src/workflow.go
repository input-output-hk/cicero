package cicero

import (
	"time"
	nomad "github.com/hashicorp/nomad/api"
)

type workflowDefinitions map[string]*workflowDefinition

type workflowDefinition struct {
	Name    string                  `json:"name"`
	Version uint64                  `json:"version"`
	Meta    map[string]interface{}  `json:"meta"`
	Steps   map[string]workflowStep `json:"steps"`
}

type workflowStep struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Job     nomad.Job              `json:"job"`
}

type Workflow struct {
	ID        uint64
	Name      string        `bun:",notnull"`
	Certs     WorkflowCerts `bun:",notnull"`
	CreatedAt time.Time     `bun:",nullzero,notnull,default:current_timestamp"`
	UpdatedAt time.Time     `bun:",nullzero,notnull,default:current_timestamp"`
}

type WorkflowCerts map[string]interface{}
