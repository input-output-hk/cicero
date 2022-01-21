package repository

import (
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/domain"
)

type RunOutputRepository interface {
	GetByRunId(uuid.UUID) (domain.RunOutput, error)
	Save(pgx.Tx, uuid.UUID, *domain.RunOutput) error
	Update(pgx.Tx, uuid.UUID, *domain.RunOutput) error
	Delete(pgx.Tx, uuid.UUID) error
}
