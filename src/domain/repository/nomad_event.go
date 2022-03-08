package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type NomadEventRepository interface {
	WithQuerier(config.PgxIface) NomadEventRepository

	Save(*domain.NomadEvent) error
	Update(*domain.NomadEvent) error
	GetByHandled(bool) ([]*domain.NomadEvent, error)
	GetLastNomadEventIndex() (uint64, error)
	GetEventAllocByNomadJobId(uuid.UUID) ([]map[string]interface{}, error)
}
