package persistence

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/jackc/pgx/v4"
)

type workflowRepository struct {
	DB domain.PgxIface
}

func NewWorkflowRepository(db domain.PgxIface) repository.WorkflowRepository {
	return workflowRepository{DB: db}
}

func (w workflowRepository) GetById(id uint64) (instance domain.WorkflowInstance, err error) {
	err = pgxscan.Get(
		context.Background(), w.DB, &instance,
		`SELECT * FROM workflow_instances WHERE id = $1`,
		id,
	)
	return
}

func (w workflowRepository) GetSummary() (summary domain.WorkflowSummary, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &summary,
		`SELECT name, (SELECT COUNT(*) FROM workflow_instances WHERE name = wf_i.name) AS num_instances, COUNT(DISTINCT source) AS num_sources FROM workflow_instances wf_i GROUP BY name`,
	)
	return
}

func (w workflowRepository) GetAll() (instances []*domain.WorkflowInstance, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &instances,
		`SELECT * FROM workflow_instances ORDER BY id DESC`,
	)
	return
}

func (w workflowRepository) GetAllByName(name string) (instances []*domain.WorkflowInstance, err error) {
	err = pgxscan.Select(
		context.Background(), w.DB, &instances,
		`SELECT * FROM workflow_instances WHERE name = $1 ORDER BY id DESC`,
		name,
	)
	return
}

func (w workflowRepository) Update(tx pgx.Tx, workflow domain.WorkflowInstance) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE workflow_instances SET facts = $2, updated_at = $3 WHERE id = $1`,
		workflow.ID, workflow.Facts, workflow.UpdatedAt,
	)
	return
}

func (w workflowRepository) Save(tx pgx.Tx, workflow *domain.WorkflowInstance) error {
	return tx.QueryRow(
		context.Background(),
		`INSERT INTO workflow_instances (source, name, facts) VALUES ($1, $2, $3) RETURNING id, created_at`,
		workflow.Source, workflow.Name, workflow.Facts,
	).Scan(&workflow.ID, &workflow.CreatedAt)
}
