package persistence

import (
	"context"
	"encoding/json"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type actionRepository struct {
	DB config.PgxIface
}

func NewActionRepository(db config.PgxIface) repository.ActionRepository {
	return &actionRepository{DB: db}
}

func (a *actionRepository) GetById(id uuid.UUID) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM actions WHERE id = $1`,
		id,
	)
	return
}

func (a *actionRepository) GetLatestByName(name string) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT DISTINCT ON (name) * FROM actions WHERE name = $1 ORDER BY name, created_at DESC`,
		name,
	)
	return
}

func (a *actionRepository) GetAll() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT * FROM actions ORDER BY created_at DESC`,
	)
	return
}

func (a *actionRepository) Save(tx pgx.Tx, action *domain.Action) error {
	if inputs, err := json.Marshal(action.Inputs); err != nil {
		return err
	} else {
		var sql string
		if action.ID == (uuid.UUID{}) {
			sql = `INSERT INTO actions (    name, source, inputs) VALUES (    $2, $3, $4) RETURNING id, created_at`
		} else {
			sql = `INSERT INTO actions (id, name, source, inputs) VALUES ($1, $2, $3, $4) RETURNING id, created_at`
		}
		return tx.QueryRow(
			context.Background(),
			sql,
			action.ID, action.Name, action.Source, inputs,
		).Scan(&action.ID, &action.CreatedAt)
	}
}

func (a *actionRepository) GetCurrent() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT DISTINCT ON (name) * FROM actions ORDER BY name, created_at DESC`,
	)
	return
}
