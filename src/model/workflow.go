package model

import (
	"time"

	nomad "github.com/hashicorp/nomad/api"

	"github.com/google/uuid"
)

type WorkflowDefinitions map[string]*WorkflowDefinition

type WorkflowDefinition struct {
	Name    string                     `json:"name"`
	Source  string                     `json:"source"`
	Meta    map[string]interface{}     `json:"meta"`
	Actions map[string]*WorkflowAction `json:"actions"`
}

type WorkflowAction struct {
	Failure Facts           `json:"failure"`
	Success Facts           `json:"success"`
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
	Source    string
	Facts     Facts
	CreatedAt *time.Time
	UpdatedAt *time.Time
}

type Facts map[string]interface{}

type ActionInstance struct {
	ID                 uuid.UUID
	WorkflowInstanceId uint64
	Name               string
	Facts              Facts
	CreatedAt          *time.Time
	UpdatedAt          *time.Time
	FinishedAt         *time.Time
}
