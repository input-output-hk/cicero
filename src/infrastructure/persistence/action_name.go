package persistence

import (
	"context"

	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type actionNameRepository struct {
	DB config.PgxIface
}

func NewActionNameRepository(db config.PgxIface) repository.ActionNameRepository {
	return &actionNameRepository{db}
}

func (a *actionNameRepository) WithQuerier(querier config.PgxIface) repository.ActionNameRepository {
	return &actionNameRepository{querier}
}

func (a *actionNameRepository) GetByFactId(id uuid.UUID) (*domain.ActionName, error) {
	actionName, err := get(
		a.DB, &domain.ActionName{},
		`SELECT * FROM action_name WHERE name = (
			SELECT name
			FROM action
			INNER JOIN invocation ON invocation.action_id = action.id
			INNER JOIN run        ON run.invocation_id    = invocation.id
			INNER JOIN fact       ON fact.run_id          = run.nomad_job_id
			WHERE fact.id = $1
		)`,
		id,
	)
	if actionName == nil {
		return nil, err
	}
	return actionName.(*domain.ActionName), err
}

func (a *actionNameRepository) GetByRunId(id uuid.UUID) (*domain.ActionName, error) {
	actionName, err := get(
		a.DB, &domain.ActionName{},
		`SELECT * FROM action_name WHERE name = (
			SELECT name
			FROM action
			INNER JOIN invocation ON invocation.action_id = action.id
			INNER JOIN run        ON run.invocation_id    = invocation.id
			WHERE run.nomad_job_id = $1
		)`,
		id,
	)
	if actionName == nil {
		return nil, err
	}
	return actionName.(*domain.ActionName), err
}

func (a *actionNameRepository) GetByInvocationId(id uuid.UUID) (*domain.ActionName, error) {
	actionName, err := get(
		a.DB, &domain.ActionName{},
		`SELECT * FROM action_name WHERE name = (
			SELECT name
			FROM action
			INNER JOIN invocation ON invocation.action_id = action.id
			WHERE invocation.id = $1
		)`,
		id,
	)
	if actionName == nil {
		return nil, err
	}
	return actionName.(*domain.ActionName), err
}

func (a *actionNameRepository) GetByActionId(id uuid.UUID) (*domain.ActionName, error) {
	actionName, err := get(
		a.DB, &domain.ActionName{},
		`SELECT * FROM action_name WHERE name = (
			SELECT name
			FROM action
			WHERE id = $1
		)`,
		id,
	)
	if actionName == nil {
		return nil, err
	}
	return actionName.(*domain.ActionName), err
}

func (a *actionNameRepository) Get(name string) (*domain.ActionName, error) {
	actionName, err := get(
		a.DB, &domain.ActionName{},
		`SELECT * FROM action_name WHERE name = $1`,
		name,
	)
	if actionName == nil {
		return nil, err
	}
	return actionName.(*domain.ActionName), err
}

func (a *actionNameRepository) Save(actionName domain.ActionName) (err error) {
	_, err = a.DB.Exec(context.Background(), `
		INSERT INTO action_name (name, active, private) VALUES ($1, $2, $3)
		ON CONFLICT (name) DO UPDATE SET active = EXCLUDED.active, private = EXCLUDED.private
	`, actionName.Name, actionName.Active, actionName.Private)
	return
}
