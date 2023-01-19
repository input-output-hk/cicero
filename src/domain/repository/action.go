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
	GetCurrentByActive(bool) ([]domain.Action, error)
	Save(*domain.Action) error
	SetActive(string, bool) error
	GetSatisfactions(uuid.UUID) (map[string]uuid.UUID, error)
	SaveSatisfaction(uuid.UUID, string, uuid.UUID) error
	DeleteSatisfaction(uuid.UUID, string) error
	HaveSatisfactionsChangedSinceLastInvocation(uuid.UUID) (bool, error)
}
