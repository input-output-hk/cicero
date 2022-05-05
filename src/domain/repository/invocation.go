package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type InvocationRepository interface {
	WithQuerier(config.PgxIface) InvocationRepository

	GetById(uuid.UUID) (domain.Invocation, error)
	GetByActionId(uuid.UUID, *Page) ([]*domain.Invocation, error)
	GetLatestByActionId(uuid.UUID) (domain.Invocation, error)
	GetInputFactIdsById(uuid.UUID) (map[string]uuid.UUID, error)
	GetAll(*Page) ([]*domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, *bool, *Page) ([]*domain.Invocation, error)
	Save(*domain.Invocation, map[string]*domain.Fact) error
	Update(*domain.Invocation) error
}
