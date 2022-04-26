package persistence

import (
	"context"
	"strconv"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/jackc/pgx/v4"
)

type invocationRepository struct {
	db config.PgxIface
}

func NewInvocationRepository(db config.PgxIface) repository.InvocationRepository {
	return &invocationRepository{db}
}

func (self *invocationRepository) WithQuerier(querier config.PgxIface) repository.InvocationRepository {
	return &invocationRepository{querier}
}

func (self *invocationRepository) GetById(id uuid.UUID) (invocation domain.Invocation, err error) {
	return invocation, pgxscan.Get(
		context.Background(), self.db, &invocation,
		`SELECT * FROM invocation WHERE id = $1`,
		id,
	)
}

func (self *invocationRepository) GetByActionId(id uuid.UUID, page *repository.Page) ([]*domain.Invocation, error) {
	invocations := make([]*domain.Invocation, page.Limit)
	return invocations, fetchPage(
		self.db, page, &invocations,
		`*`, `invocation WHERE action_id = $1`, `created_at DESC`,
		id,
	)
}

func (self *invocationRepository) GetLatestByActionId(id uuid.UUID) (invocation domain.Invocation, err error) {
	return invocation, pgxscan.Get(
		context.Background(), self.db, &invocation,
		`SELECT DISTINCT ON (action_id) * FROM invocation WHERE action_id = $1 ORDER BY action_id, created_at DESC`,
		id,
	)
}

func (self *invocationRepository) GetInputFactIdsById(id uuid.UUID) (inputFactIds repository.InvocationInputFactIds, err error) {
	inputs := []struct {
		InputName string    `json:"input_name"`
		FactId    uuid.UUID `json:"fact_id"`
	}{}

	err = pgxscan.Select(
		context.Background(), self.db, &inputs,
		`SELECT input_name, fact_id
		FROM invocation_inputs
		WHERE invocation_id = $1`,
		id,
	)
	if err != nil {
		return
	}

	inputFactIds = map[string][]uuid.UUID{}
	for _, runInput := range inputs {
		if _, exists := inputFactIds[runInput.InputName]; !exists {
			inputFactIds[runInput.InputName] = []uuid.UUID{}
		}
		inputFactIds[runInput.InputName] = append(inputFactIds[runInput.InputName], runInput.FactId)
	}

	return
}

func (self *invocationRepository) GetAll(page *repository.Page) ([]*domain.Invocation, error) {
	invocations := make([]*domain.Invocation, page.Limit)
	return invocations, fetchPage(
		self.db, page, &invocations,
		`*`, `invocation`, `created_at DESC`,
	)
}

// `ok`: Allows to filter for successful or failed invocations.
func (self *invocationRepository) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, ok *bool, page *repository.Page) ([]*domain.Invocation, error) {
	joins := ``
	for i := range factIds {
		iStr := strconv.Itoa(i + 1)
		joins += ` JOIN invocation_inputs AS invocation_inputs_` + iStr + ` ON
			invocation_inputs_` + iStr + `.invocation_id = invocation.id AND
			invocation_inputs_` + iStr + `.fact_id = $` + iStr
	}

	args := make([]interface{}, len(factIds))
	for i, factId := range factIds {
		args[i] = factId
	}

	var whereOk string
	if ok == nil {
		whereOk = ``
	} else {
		whereOk = `WHERE `
		if !*ok {
			whereOk += `NOT `
		}
		whereOk += `EXISTS (
			SELECT NULL
			FROM run
			WHERE invocation_id = invocation.id
		)`
	}

	var from string
	if recursive {
		from = `(
			SELECT * FROM (
				WITH RECURSIVE invocations AS (
					SELECT invocation.*
					FROM invocation
					` + joins + `

					UNION

					SELECT invocation.*
					FROM invocation
					JOIN invocations ON
						EXISTS (
							SELECT NULL
							FROM invocation_inputs
							JOIN run ON run.invocation_id = invocations.id
							JOIN fact ON
								fact.id = invocation_inputs.fact_id AND
								fact.run_id = run.nomad_job_id
							WHERE invocation_inputs.invocation_id = invocation.id
						)
				) SELECT * FROM invocations
			) AS invocation ` + whereOk + `
		) AS invocation`
	} else {
		from = `(SELECT invocation.* FROM invocation ` + joins + whereOk + `) AS invocation`
	}

	invocations := make([]*domain.Invocation, page.Limit)
	return invocations, fetchPage(
		self.db, page, &invocations,
		`invocation.*`, from, `created_at`,
		args...,
	)
}

func (self *invocationRepository) Save(invocation *domain.Invocation, inputs map[string]interface{}) error {
	ctx := context.Background()

	if err := self.db.BeginFunc(ctx, func(tx pgx.Tx) error {
		if err := tx.QueryRow(
			ctx,
			`INSERT INTO invocation (action_id) VALUES ($1) RETURNING id, created_at`,
			invocation.ActionId,
		).Scan(&invocation.Id, &invocation.CreatedAt); err != nil {
			return err
		}

		if len(inputs) > 0 {
			sql := `INSERT INTO invocation_inputs (invocation_id, input_name, fact_id) VALUES`
			args := []interface{}{}

			i := 1
			for name, factOrFacts := range inputs {
				addRow := func(factId uuid.UUID) {
					if i > 1 {
						sql += `, `
					}

					sql += `($` + strconv.Itoa(i) + `, $` + strconv.Itoa(i+1) + `, $` + strconv.Itoa(i+2) + `)`
					args = append(args, invocation.Id, name, factId)
					i += 3
				}

				switch factOrFactsTyped := factOrFacts.(type) {
				case *domain.Fact:
					addRow(factOrFactsTyped.ID)
				case []*domain.Fact:
					for _, fact := range factOrFactsTyped {
						addRow(fact.ID)
					}
				default:
					panic("inputs must be pointer to a Fact or array")
				}
			}

			if _, err := tx.Exec(ctx, sql, args...); err != nil {
				return err
			}
		}

		return nil
	}); err != nil {
		return err
	}

	return nil
}

func (self *invocationRepository) Update(invocation *domain.Invocation) (err error) {
	_, err = self.db.Exec(
		context.Background(),
		`UPDATE invocation SET eval_stdout = $2, eval_stderr = $3 WHERE id = $1`,
		invocation.Id, invocation.EvalStdout, invocation.EvalStderr,
	)
	return
}
