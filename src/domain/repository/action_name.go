package repository

import (
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionNameRepository interface {
	WithQuerier(config.PgxIface) ActionNameRepository

	GetByActionId(uuid.UUID) (*domain.ActionName, error)
	GetByInvocationId(uuid.UUID) (*domain.ActionName, error)
	GetByFactId(uuid.UUID) (*domain.ActionName, error)
	GetByRunId(uuid.UUID) (*domain.ActionName, error)
	Get(string) (*domain.ActionName, error)
	Save(domain.ActionName) error
}
