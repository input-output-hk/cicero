package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

type RunRepository interface {
	WithQuerier(config.PgxIface) RunRepository

	GetByNomadJobId(uuid.UUID) (*domain.Run, error)
	GetByNomadJobIdWithLock(uuid.UUID, string) (*domain.Run, error)
	GetByInvocationId(uuid.UUID) (*domain.Run, error)
	GetByActionId(uuid.UUID, *Page) ([]domain.Run, error)
	GetLatestByActionId(uuid.UUID) (*domain.Run, error)
	GetAll(*Page) ([]domain.Run, error)
	GetByPrivate(*Page, util.MayBool) ([]domain.Run, error)
	Save(*domain.Run) error
	Update(*domain.Run) error
}
