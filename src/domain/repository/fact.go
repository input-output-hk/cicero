package repository

import (
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/domain"
)

type FactRepository interface {
	GetById(uuid.UUID) (domain.Fact, error)
	GetLatestByFields([][]string) (domain.Fact, error)
	GetByFields([][]string) ([]*domain.Fact, error)
	Save(pgx.Tx, *domain.Fact) error
	GetLatestOutputByActionId(uuid.UUID) (domain.Fact, error)
}
