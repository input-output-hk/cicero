package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

type ActionRepository interface {
	WithQuerier(config.PgxIface) ActionRepository

	GetById(uuid.UUID) (*domain.Action, error)
	GetByInvocationId(uuid.UUID) (*domain.Action, error)
	GetByRunId(uuid.UUID) (*domain.Action, error)
	GetLatestByName(string) (*domain.Action, error)
	Get(*Page, ActionGetOpts) ([]domain.Action, error)
	Save(*domain.Action) error
	GetSatisfactions(uuid.UUID) (map[string]uuid.UUID, error)
	SaveSatisfaction(uuid.UUID, string, uuid.UUID) error
	DeleteSatisfaction(uuid.UUID, string) error
	HaveSatisfactionsChangedSinceLastInvocation(uuid.UUID) (bool, error)
}

type ActionGetOpts struct {
	Name    *string
	Current bool
	Active  util.MayBool
	Private util.MayBool
}
