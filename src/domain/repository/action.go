package repository

import (
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/domain"
)

type ActionRepository interface {
	GetById(uuid.UUID) (domain.Action, error)
	GetByRunId(uuid.UUID) (domain.Action, error)
	GetLatestByName(string) (domain.Action, error)
	GetAll() ([]*domain.Action, error)
	GetCurrent(pgx.Tx) ([]*domain.Action, error)
	Save(pgx.Tx, *domain.Action) error
}
