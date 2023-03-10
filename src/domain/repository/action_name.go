package repository

import (
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionNameRepository interface {
	WithQuerier(config.PgxIface) ActionNameRepository

	GetByActionId(uuid.UUID) (*domain.ActionName, error)
	Get(string) (*domain.ActionName, error)
	Save(domain.ActionName) error
}
