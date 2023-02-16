package repository

import (
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

type NomadEventRepository interface {
	WithQuerier(config.PgxIface) NomadEventRepository

	Save(*domain.NomadEvent) error
	Update(*domain.NomadEvent) error
	GetByHandled(bool) ([]domain.NomadEvent, error)
	GetLastNomadEventIndex() (uint64, error)
	GetEventAllocationByJobId(uuid.UUID) ([]nomad.Allocation, error)
	GetLatestEventAllocationByJobId(uuid.UUID) ([]nomad.Allocation, error)
	GetLatestEventAllocationById(uuid.UUID) (*nomad.Allocation, error)
}
