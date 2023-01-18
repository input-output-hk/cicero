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

func (a *actionRepository) SetActive(name string, active bool) (err error) {
	var sql string
	if active {
		sql = `INSERT INTO action_active (name) VALUES ($1) ON CONFLICT DO NOTHING`
	} else {
		sql = `DELETE FROM action_active WHERE name = $1`
	}
	_, err = a.DB.Exec(
		context.Background(),
		sql,
		name,
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

func (a *actionRepository) GetCurrentByActive(active bool) (actions []domain.Action, err error) {
	sql := `
		SELECT DISTINCT ON (name) *
		FROM action
		WHERE
	`
	if !active {
		sql += ` NOT `
	}
	sql += `
		EXISTS (
			SELECT NULL
			FROM action_active
			WHERE name = action.name
		)
		ORDER BY name, created_at DESC
	`

	actions = []domain.Action{}
	err = pgxscan.Select(
		context.Background(), a.DB, &actions,
		sql,
	)
	return
}

func (a *actionRepository) GetSatisfactions(id uuid.UUID) (inputs map[string]uuid.UUID, err error) {
	inputs = map[string]uuid.UUID{}

	results := []struct {
		InputName string    `json:"input_name"`
		FactId    uuid.UUID `json:"fact_id"`
	}{}

	err = pgxscan.Select(
		context.Background(), a.DB, &results,
		`SELECT input_name, fact_id
		FROM action_satisfaction
		WHERE action_id = $1`,
		id,
	)
	if err != nil {
		return
	}

	for _, result := range results {
		if _, exists := inputs[result.InputName]; exists {
			panic("This should never happen™")
		}
		inputs[result.InputName] = result.FactId
	}

	return
}

func (a *actionRepository) SaveSatisfaction(actionId uuid.UUID, input string, factId uuid.UUID) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`INSERT INTO action_satisfaction (action_id, input_name, fact_id) VALUES ($1, $2, $3)
		ON CONFLICT (action_id, input_name) DO UPDATE SET fact_id = EXCLUDED.fact_id`,
		actionId, input, factId,
	)
	return
}

func (a *actionRepository) DeleteSatisfaction(actionId uuid.UUID, input string) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`DELETE FROM action_satisfaction
		WHERE action_id = $1 AND input_name = $2`,
		actionId, input,
	)
	return
}

func (a *actionRepository) DeleteSatisfactions(actionId uuid.UUID) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`DELETE FROM action_satisfaction
		WHERE action_id = $1`,
		actionId,
	)
	return
}
