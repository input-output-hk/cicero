package repository

import (
	"io"

	"cuelang.org/go/cue"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type FactRepository interface {
	WithQuerier(config.PgxIface) FactRepository

	GetById(uuid.UUID) (*domain.Fact, error)
	GetByIds(map[string]uuid.UUID) (map[string]domain.Fact, error)
	GetByRunId(uuid.UUID) ([]domain.Fact, error)
	GetBinaryById(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	GetLatestByCue(cue.Value) (*domain.Fact, error)
	GetByCue(cue.Value) ([]domain.Fact, error)
	Save(*domain.Fact, io.Reader) error
}
