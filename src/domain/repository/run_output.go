package repository

import (
	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type RunOutputRepository interface {
	WithQuerier(config.PgxIface) RunOutputRepository

	GetByRunId(uuid.UUID) (domain.RunOutput, error)
	Save(uuid.UUID, *domain.RunOutput) error
	Update(uuid.UUID, *domain.RunOutput) error
	Delete(uuid.UUID) error
}
