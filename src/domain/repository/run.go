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
	GetInputFactIdsByNomadJobId(uuid.UUID) (RunInputFactIds, error)
	GetAll(*Page) ([]*domain.Run, error)
	GetByInputFactIds([]*uuid.UUID, bool, *Page) ([]*domain.Run, error)
	Save(*domain.Run, map[string]interface{}) error
	Update(*domain.Run) error
}

type RunInputFactIds map[string][]uuid.UUID

func (self *RunInputFactIds) MapStringInterface(inputs map[string]domain.InputDefinition) (map[string]interface{}, error) {
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
