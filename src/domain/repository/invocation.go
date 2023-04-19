package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

type InvocationRepository interface {
	WithQuerier(config.PgxIface) InvocationRepository

	GetById(uuid.UUID) (*domain.Invocation, error)
	GetByActionId(uuid.UUID, *Page) ([]domain.Invocation, error)
	GetLatestByActionId(uuid.UUID) (*domain.Invocation, error)
	GetInputFactIdsById(uuid.UUID) (map[string]uuid.UUID, error)
	GetAll(*Page) ([]domain.Invocation, error)
	GetByPrivate(*Page, util.MayBool) ([]domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, util.MayBool, *Page) ([]domain.Invocation, error)
	Save(*domain.Invocation, map[string]domain.Fact) error
	End(uuid.UUID) error
}
