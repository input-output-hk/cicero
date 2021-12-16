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
	err = pgxscan.Select(
		context.Background(), a.DB, &action,
		`SELECT * FROM actions WHERE name = $1 GROUP BY name HAVING created_at = MAX(created_at)`,
		name,
	)
	return
}

func (a *actionRepository) GetAll() (instances []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &instances,
		`SELECT * FROM actions ORDER BY created_at DESC`,
	)
	return
}

func (a *actionRepository) Save(tx pgx.Tx, action *domain.Action) error {
	if inputs, err := json.Marshal(action.Inputs); err != nil {
		return err
	} else {
		return tx.QueryRow(
			context.Background(),
			`INSERT INTO actions (name, source, inputs) VALUES ($1, $2) RETURNING id`,
			action.Name, action.Source, inputs,
		).Scan(&action.ID)
	}
}

func (a *actionRepository) GetCurrent() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT * FROM actions GROUP BY name HAVING created_at = MAX(created_at)`,
	)
	return
}
