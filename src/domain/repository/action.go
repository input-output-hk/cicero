package repository

import (
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/jackc/pgx/v4"
)

type ActionRepository interface {
	GetById(uuid.UUID) (domain.ActionInstance, error)
	GetByNameAndWorkflowId(string, uint64) (domain.ActionInstance, error)
	GetAll() ([]*domain.ActionInstance, error)
	Save(pgx.Tx, *domain.ActionInstance) error
	Update(pgx.Tx, domain.ActionInstance) error
}
