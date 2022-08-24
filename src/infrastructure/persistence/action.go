package persistence

import (
	"context"

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
	return &actionRepository{db}
}

func (a *actionRepository) WithQuerier(querier config.PgxIface) repository.ActionRepository {
	return &actionRepository{querier}
}

func (a *actionRepository) GetById(id uuid.UUID) (*domain.Action, error) {
	action, err := get(
		a.DB, &domain.Action{},
		`SELECT * FROM action WHERE id = $1`,
		id,
	)
	if action == nil {
		return nil, err
	}
	return action.(*domain.Action), err
}

func (a *actionRepository) GetByInvocationId(id uuid.UUID) (*domain.Action, error) {
	action, err := get(
		a.DB, &domain.Action{},
		`SELECT * FROM action WHERE EXISTS (
			SELECT NULL
			FROM invocation
			WHERE
				invocation.id = $1 AND 
				invocation.action_id = action.id
		)`,
		id,
	)
	if action == nil {
		return nil, err
	}
	return action.(*domain.Action), err
}

func (a *actionRepository) GetByRunId(id uuid.UUID) (*domain.Action, error) {
	action, err := get(
		a.DB, &domain.Action{},
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
	if action == nil {
		return nil, err
	}
	return action.(*domain.Action), err
}

func (a *actionRepository) GetByName(name string, page *repository.Page) ([]domain.Action, error) {
	actions := make([]domain.Action, page.Limit)
	return actions, fetchPage(
		a.DB, page, &actions,
		`*`, `action WHERE name = $1`, `created_at DESC`,
		name,
	)
}

func (a *actionRepository) GetLatestByName(name string) (*domain.Action, error) {
	action, err := get(
		a.DB, &domain.Action{},
		`SELECT DISTINCT ON (name) * FROM action WHERE name = $1 ORDER BY name, created_at DESC`,
		name,
	)
	if action == nil {
		return nil, err
	}
	return action.(*domain.Action), err
}

func (a *actionRepository) GetAll() (actions []domain.Action, err error) {
	actions = []domain.Action{}
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT * FROM action ORDER BY created_at DESC`,
	)
	return
}

func (a *actionRepository) Save(action *domain.Action) error {
	var sql string
	if action.ID == (uuid.UUID{}) {
		sql = `INSERT INTO action (    name, source, io) VALUES (    $2, $3, $4) RETURNING id, created_at`
	} else {
		sql = `INSERT INTO action (id, name, source, io) VALUES ($1, $2, $3, $4) RETURNING id, created_at`
	}
	return a.DB.QueryRow(
		context.Background(),
		sql,
		action.ID, action.Name, action.Source, action.InOut,
	).Scan(&action.ID, &action.CreatedAt)
}

func (a *actionRepository) Update(action *domain.Action) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`UPDATE action SET active = $2 WHERE id = $1`,
		action.ID, action.Active,
	)
	return
}

func (a *actionRepository) GetCurrent() (actions []domain.Action, err error) {
	actions = []domain.Action{}
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT DISTINCT ON (name) * FROM action ORDER BY name, created_at DESC`,
	)
	return
}

func (a *actionRepository) GetCurrentActive() (actions []domain.Action, err error) {
	actions = []domain.Action{}
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		`SELECT DISTINCT ON (name) * FROM action WHERE active ORDER BY name, created_at DESC`,
	)
	return
}
