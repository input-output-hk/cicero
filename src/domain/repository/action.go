package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type ActionRepository interface {
	WithQuerier(config.PgxIface) ActionRepository

	GetById(uuid.UUID) (*domain.Action, error)
	GetByInvocationId(uuid.UUID) (*domain.Action, error)
	GetByRunId(uuid.UUID) (*domain.Action, error)
	GetByName(string, *Page) ([]domain.Action, error)
	GetLatestByName(string) (*domain.Action, error)
	GetAll() ([]domain.Action, error)
	GetCurrent() ([]domain.Action, error)
	GetCurrentActive() ([]domain.Action, error)
	Save(*domain.Action) error
	Update(*domain.Action) error
}
