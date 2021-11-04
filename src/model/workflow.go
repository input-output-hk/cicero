package model

import (
	nomad "github.com/hashicorp/nomad/api"
	"time"

	"github.com/google/uuid"
)

type WorkflowDefinitions map[string]*WorkflowDefinition

type WorkflowDefinition struct {
	Name    string                    `json:"name"`
	Version uint64                    `json:"version"`
	Meta    map[string]interface{}    `json:"meta"`
	Actions map[string]WorkflowAction `json:"actions"`
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

