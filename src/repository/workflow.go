package repository

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type workflowRepository struct {
	DB *pgxpool.Pool
}

type WorkflowRepository interface {
	GetAll() ([]*model.WorkflowInstance, error)
	GetAllByName(string) ([]*model.WorkflowInstance, error)
	GetById(uint64) (model.WorkflowInstance, error)
	Save(pgx.Tx, *model.WorkflowInstance) error
	Update(pgx.Tx, uint64, model.WorkflowInstance) error
}

func NewWorkflowRepository(db *pgxpool.Pool) WorkflowRepository {
	return workflowRepository{
		DB: db,
	}
}

func (w workflowRepository) GetById(id uint64) (instance model.WorkflowInstance, err error) {
	err = pgxscan.Get(
		context.Background(), w.DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		id,
	)
	return
}

func (w workflowRepository) GetAll() (instances []*model.WorkflowInstance, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &instances,
		`SELECT * FROM workflow_instances ORDER BY id DESC`,
	)
	return
}

func (w workflowRepository) GetAllByName(name string) (instances []*model.WorkflowInstance, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &instances,
		`SELECT * FROM workflow_instances WHERE name = $1 ORDER BY id DESC`,
		name,
	)
	return
}

func (w workflowRepository) Update(tx pgx.Tx, id uint64, workflow model.WorkflowInstance) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE workflow_instances SET certs = $2, updated_at = $3 WHERE id = $1`,
		id, workflow.Certs, workflow.UpdatedAt,
	)
	return
}

func (w workflowRepository) Save(tx pgx.Tx, workflow *model.WorkflowInstance) error {
	return tx.QueryRow(
		context.Background(),
		`INSERT INTO workflow_instances (source, name, certs) VALUES ($1, $2, $3) RETURNING id`,
		workflow.Source, workflow.Name, workflow.Certs,
	).Scan(&workflow.ID)
}
