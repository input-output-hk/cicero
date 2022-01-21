package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type RunRepository interface {
	WithQuerier(config.PgxIface) RunRepository

	GetByNomadJobId(uuid.UUID) (domain.Run, error)
	GetByActionId(uuid.UUID, *Page) ([]*domain.Run, error)
	GetLatestByActionId(uuid.UUID) (domain.Run, error)
	GetInputFactIdsByNomadJobId(uuid.UUID) (map[string][]uuid.UUID, error)
	GetAll(*Page) ([]*domain.Run, error)
	Save(*domain.Run, map[string]interface{}) error
	Update(*domain.Run) error
}
