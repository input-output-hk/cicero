package repository

import (
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
)

type NomadEventRepository interface {
	Save(pgx.Tx, *nomad.Event) error
	GetLastNomadEvent() (uint64, error)
	GetEventAllocByNomadJobId(uuid.UUID) ([]map[string]interface{}, error)
}
