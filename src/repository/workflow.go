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
	GetSummary() (WorkflowSummary, error)
	GetAll() ([]*model.WorkflowInstance, error)
	GetAllByName(string) ([]*model.WorkflowInstance, error)
	GetById(uint64) (model.WorkflowInstance, error)
	Save(pgx.Tx, *model.WorkflowInstance) error
	Update(pgx.Tx, model.WorkflowInstance) error
}

type WorkflowSummary []struct {
	Name         string
	NumSources   uint64
	NumInstances uint64
}

func NewWorkflowRepository(db *pgxpool.Pool) WorkflowRepository {
	return workflowRepository{DB: db}
}

func (w workflowRepository) GetById(id uint64) (instance model.WorkflowInstance, err error) {
	err = pgxscan.Get(
		context.Background(), w.DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		id,
	)
	return
}

func (w workflowRepository) GetSummary() (summary WorkflowSummary, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &summary,
		`SELECT name, (SELECT COUNT(*) FROM workflow_instances WHERE name = wf_i.name) AS num_instances, COUNT(DISTINCT source) AS num_sources FROM workflow_instances wf_i GROUP BY name`,
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

func (w workflowRepository) Update(tx pgx.Tx, workflow model.WorkflowInstance) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE workflow_instances SET certs = $2, updated_at = $3 WHERE id = $1`,
		workflow.ID, workflow.Certs, workflow.UpdatedAt,
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
