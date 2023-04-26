package persistence

import (
	"context"

	"github.com/georgysavva/scany/v2/pgxscan"
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

func (a *actionRepository) Get(page *repository.Page, opts repository.ActionGetOpts) ([]domain.Action, error) {
	var actions []domain.Action
	if page == nil {
		actions = make([]domain.Action, 0)
	} else {
		actions = make([]domain.Action, 0, page.Limit)
	}

	var selects string
	if opts.Current {
		selects = `DISTINCT ON (action.name) `
	}
	selects += `action.*`

	return actions, fetchPage(
		a.DB, page, &actions,
		selects, `action
			NATURAL JOIN action_name
			WHERE
				($1::text IS NULL OR $1::text = action.name) AND
				($2::bool IS NULL OR $2::bool = action_name.private) AND
				($3::bool IS NULL OR $3::bool = action_name.active)
		`, `name, action.created_at DESC`,
		opts.Name,
		opts.Private.Ptr(),
		opts.Active.Ptr(),
	)
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

func (a *actionRepository) HaveSatisfactionsChangedSinceLastInvocation(id uuid.UUID) (changed bool, err error) {
	_, err = get(
		a.DB, &changed,
		`
		WITH latest_invocation AS (
			SELECT DISTINCT ON (action_id) id, action_id
			FROM invocation
			WHERE action_id = $1
			ORDER BY action_id, created_at DESC
		)
		SELECT
			NOT EXISTS (
				SELECT NULL
				FROM latest_invocation
			) OR
			EXISTS (
				SELECT NULL
				FROM invocation_inputs, latest_invocation
				WHERE
					invocation_id = latest_invocation.id AND
					NOT EXISTS (
						SELECT NULL
						FROM action_satisfaction
						WHERE
							action_id = latest_invocation.action_id AND
							input_name = invocation_inputs.input_name AND
							fact_id = invocation_inputs.fact_id
					)
			)
		`,
		id,
	)
	return
}
