package repository

import (
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/jackc/pgx/v4"
)

type WorkflowRepository interface {
	GetSummary() (domain.WorkflowSummary, error)
	GetAll() ([]*domain.WorkflowInstance, error)
	GetAllByName(string) ([]*domain.WorkflowInstance, error)
	GetById(uint64) (domain.WorkflowInstance, error)
	Save(pgx.Tx, *domain.WorkflowInstance) error
	Update(pgx.Tx, domain.WorkflowInstance) error
}
