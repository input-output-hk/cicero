package repository

import (
	"io"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type FactRepository interface {
	WithQuerier(config.PgxIface) FactRepository

	GetById(uuid.UUID) (domain.Fact, error)
	GetByRunId(uuid.UUID) ([]*domain.Fact, error)
	GetBinaryById(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	GetLatestByFields([][]string) (domain.Fact, error)
	GetByFields([][]string) ([]*domain.Fact, error)
	Save(*domain.Fact, io.Reader) error
}
