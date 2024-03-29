package repository

import (
	"io"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type FactRepository interface {
	WithQuerier(config.PgxIface) FactRepository

	GetById(uuid.UUID) (*domain.Fact, error)
	GetByIds(map[string]uuid.UUID) (map[string]domain.Fact, error)
	GetByRunId(uuid.UUID) ([]domain.Fact, error)
	GetBinaryById(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	Save(*domain.Fact, io.Reader) error
}
