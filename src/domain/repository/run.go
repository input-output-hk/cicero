package repository

import (
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/domain"
)

type RunRepository interface {
	GetByNomadJobId(uuid.UUID) (domain.Run, error)
	GetByActionId(id uuid.UUID, limit int, offSet int) ([]*domain.Run, error)
	GetAll(limit int, offSet int) ([]*domain.Run, error)
	Save(pgx.Tx, *domain.Run) error
	Update(pgx.Tx, *domain.Run) error
}
