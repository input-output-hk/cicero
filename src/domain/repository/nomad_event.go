package repository

import (
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
)

type NomadEventRepository interface {
	Save(pgx.Tx, *nomad.Event) error
	GetLastNomadEvent() (uint64, error)
	GetEventAllocByWorkflowId(uint64) ([]map[string]interface{}, error)
}
