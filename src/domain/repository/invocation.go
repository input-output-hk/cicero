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
	GetInputFactIdsById(uuid.UUID) (InvocationInputFactIds, error)
	GetAll(*Page) ([]*domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, *bool, *Page) ([]*domain.Invocation, error)
	Save(*domain.Invocation, map[string]interface{}) error
	Update(*domain.Invocation) error
}

type InvocationInputFactIds map[string][]uuid.UUID

func (self *InvocationInputFactIds) MapStringInterface(inputs map[string]domain.InputDefinition) (map[string]interface{}, error) {
	result := map[string]interface{}{}
	for name, factIds := range *self {
		switch inputs[name].Select {
		case domain.InputDefinitionSelectLatest:
			if len(factIds) > 1 {
				panic("This should never happen™")
			}
			result[name] = factIds[0]
		case domain.InputDefinitionSelectAll:
			result[name] = make([]*domain.Fact, len(factIds))
			for i, factId := range factIds {
				result[name].([]uuid.UUID)[i] = factId
			}
		default:
			panic("unhandled case")
		}
	}
	return result, nil
}
