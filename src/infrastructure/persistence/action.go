package persistence

import (
	"context"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type actionRepository struct {
	DB *pgxpool.Pool
}


func NewActionRepository(db *pgxpool.Pool) repository.ActionRepository {
	return actionRepository{DB: db}
}

func (a actionRepository) GetById(id uuid.UUID) (action domain.ActionInstance, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action_instances WHERE id = $1`,
		id,
	)
	return
}

// FIXME the name is not unique in the action_instances table for a workflow_instance_id
func (a actionRepository) GetByNameAndWorkflowId(name string, workflowId uint64) (action domain.ActionInstance, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action_instances WHERE name = $1 AND workflow_instance_id = $2`,
		name, workflowId,
	)
	return
}

func (a actionRepository) GetAll() (instances []*domain.ActionInstance, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &instances,
		`SELECT * FROM action_instances ORDER BY created_at DESC`,
	)
	return
}

func (a actionRepository) Save(tx pgx.Tx, action *domain.ActionInstance) error {
	return tx.QueryRow(
		context.Background(),
		`INSERT INTO action_instances (workflow_instance_id, name, facts) VALUES ($1, $2, $3) RETURNING id`,
		action.WorkflowInstanceId, action.Name, action.Facts,
	).Scan(&action.ID)
}

func (a actionRepository) Update(tx pgx.Tx, action domain.ActionInstance) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE action_instances SET finished_at = $2, updated_at = $3, facts = $4 WHERE id = $1`,
		action.ID, action.FinishedAt, action.UpdatedAt, action.Facts,
	)
	return
}
