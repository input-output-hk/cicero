package repository

import (
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/config"
)

type NomadEventRepository interface {
	WithQuerier(config.PgxIface) NomadEventRepository

	Save(*nomad.Event) error
	GetLastNomadEvent() (uint64, error)
	GetEventAllocByNomadJobId(uuid.UUID) ([]map[string]interface{}, error)
}
