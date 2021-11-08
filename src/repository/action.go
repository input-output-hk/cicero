package repository

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type actionRepository struct {
	DB *pgxpool.Pool
}

type ActionRepository interface {
	GetById(uuid.UUID)(*model.ActionInstance, error)
	GetByNameAndWorkflowId(string, uint64)(*model.ActionInstance, error)
	GetAll()([]*model.ActionInstance, error)
	Save(pgx.Tx, *model.ActionInstance) error
	Update(pgx.Tx, uuid.UUID, *model.ActionInstance)(pgconn.CommandTag, error)
}

func NewActionRepository(db *pgxpool.Pool) ActionRepository {
	return actionRepository{DB: db}
}

func (a actionRepository) GetById(id uuid.UUID) (action *model.ActionInstance, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action_instances WHERE id = $1`,
		&id,
	)
	return action, err
}

//TODO: Fixme, the name is not unique in the action_instances table for a workflow_instance_id
func (a actionRepository) GetByNameAndWorkflowId(name string, workflowId uint64) (action *model.ActionInstance, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action_instances WHERE name = $1 AND workflow_instance_id = $2`,
		&name, &workflowId,
	)
	return action, err
}

func (a actionRepository) GetAll() (instances []*model.ActionInstance, err error) {
	err = pgxscan.Select(
		context.Background(),
		a.DB,
		&instances,
		`SELECT * FROM action_instances ORDER BY created_at DESC`)
	return instances, err
}

func (a actionRepository) Save(tx pgx.Tx, action *model.ActionInstance) error {
	return tx.QueryRow(context.Background(),
		`INSERT INTO action_instances (workflow_instance_id, name, certs) VALUES ($1, $2, $3) RETURNING id`,
		&action.WorkflowInstanceId, &action.Name, &action.Certs).
		Scan(&action.ID)
}

func (a actionRepository) Update(tx pgx.Tx, id uuid.UUID, action *model.ActionInstance) (pgconn.CommandTag, error) {
	return tx.Exec(
		context.Background(),
		`UPDATE action_instances SET finished_at = $2, updated_at = $3, certs = $4 WHERE id = $1`,
		&id, &action.FinishedAt, &action.UpdatedAt, &action.Certs,
	)
}
