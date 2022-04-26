package persistence

import (
	"context"
	"encoding/json"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"

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

func (a *actionRepository) WithQuerier(querier config.PgxIface) repository.ActionRepository {
	return &actionRepository{
		DB: querier,
	}
}

func (a *actionRepository) GetById(id uuid.UUID) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action WHERE id = $1`,
		id,
	)
	return
}

func (a *actionRepository) GetByInvocationId(id uuid.UUID) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action WHERE EXISTS (
			SELECT NULL
			FROM invocation
			WHERE
				invocation.id = $1 AND 
				invocation.action_id = action.id
		)`,
		id,
	)
	return
}

func (a *actionRepository) GetByRunId(id uuid.UUID) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT * FROM action WHERE EXISTS (
			SELECT NULL
			FROM run
			JOIN invocation ON
				invocation.id = run.invocation_id AND
				invocation.action_id = action.id
			WHERE run.nomad_job_id = $1
		)`,
		id,
	)
	return
}

func (a *actionRepository) GetByName(name string, page *repository.Page) ([]*domain.Action, error) {
	actions := make([]*domain.Action, page.Limit)
	return actions, fetchPage(
		a.DB, page, &actions,
		`*`, `action WHERE name = $1`, `created_at DESC`,
		name,
	)
}

func (a *actionRepository) GetLatestByName(name string) (action domain.Action, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &action,
		`SELECT DISTINCT ON (name) * FROM action WHERE name = $1 ORDER BY name, created_at DESC`,
		name,
	)
	return
}

func (a *actionRepository) GetAll() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT * FROM action ORDER BY created_at DESC`,
	)
	return
}

func (a *actionRepository) Save(action *domain.Action) error {
	if inputs, err := json.Marshal(action.Inputs); err != nil {
		return err
	} else {
		var sql string
		if action.ID == (uuid.UUID{}) {
			sql = `INSERT INTO action (    name, source, inputs) VALUES (    $2, $3, $4) RETURNING id, created_at`
		} else {
			sql = `INSERT INTO action (id, name, source, inputs) VALUES ($1, $2, $3, $4) RETURNING id, created_at`
		}
		return a.DB.QueryRow(
			context.Background(),
			sql,
			action.ID, action.Name, action.Source, inputs,
		).Scan(&action.ID, &action.CreatedAt)
	}
}

func (a *actionRepository) Update(action *domain.Action) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`UPDATE action SET active = $2 WHERE id = $1`,
		action.ID, action.Active,
	)
	return
}

func (a *actionRepository) GetCurrent() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT DISTINCT ON (name) * FROM action ORDER BY name, created_at DESC`,
	)
	return
}

func (a *actionRepository) GetCurrentActive() (actions []*domain.Action, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT DISTINCT ON (name) * FROM action WHERE active ORDER BY name, created_at DESC`,
	)
	return
}
