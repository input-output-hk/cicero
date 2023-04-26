package persistence

import (
	"context"
	"strconv"
	"time"

	"github.com/georgysavva/scany/v2/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/util"
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

func (self *invocationRepository) GetById(id uuid.UUID) (*domain.Invocation, error) {
	invocation, err := get(
		self.db, &domain.Invocation{},
		`SELECT * FROM invocation WHERE id = $1`,
		id,
	)
	if invocation == nil {
		return nil, err
	}
	return invocation.(*domain.Invocation), err
}

func (self *invocationRepository) GetLatestByActionId(id uuid.UUID) (*domain.Invocation, error) {
	invocation, err := get(
		self.db, &domain.Invocation{},
		`SELECT DISTINCT ON (action_id) * FROM invocation WHERE action_id = $1 ORDER BY action_id, created_at DESC`,
		id,
	)
	if invocation == nil {
		return nil, err
	}
	return invocation.(*domain.Invocation), err
}

func (self *invocationRepository) GetInputFactIdsById(id uuid.UUID) (inputFactId map[string]uuid.UUID, err error) {
	inputFactId = map[string]uuid.UUID{}
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

	for _, runInput := range inputs {
		if _, exists := inputFactId[runInput.InputName]; exists {
			panic("This should never happen™")
		}
		inputFactId[runInput.InputName] = runInput.FactId
	}

	return
}

func (self *invocationRepository) Get(page *repository.Page, opts repository.InvocationGetOpts) ([]domain.Invocation, error) {
	var invocations []domain.Invocation
	if page == nil {
		invocations = make([]domain.Invocation, 0)
	} else {
		invocations = make([]domain.Invocation, 0, page.Limit)
	}
	return invocations, fetchPage(
		self.db, page, &invocations,
		`invocation.*`, `invocation
			JOIN action ON action.id = invocation.action_id
			JOIN action_name ON action_name.name = action.name
			LEFT JOIN run ON run.invocation_id = invocation.id
			WHERE
				($1::bool IS NULL OR $1::bool = action_name.private) AND
				($2::bool IS NULL OR $2::bool = (invocation.finished_at IS NULL)) AND
				($3::bool IS NULL OR $3::bool = (run IS NULL AND invocation.finished_at IS NOT NULL)) AND
				(
					$4::bool IS NULL OR
					$4::bool AND run.status = 'running' OR
					NOT $4::bool AND (run.status != 'running' OR run IS NULL)
				) AND
				(
					$5::bool IS NULL OR
					$5::bool AND run.status = 'succeeded' OR
					NOT $5::bool AND (run.status != 'succeeded' OR run IS NULL)
				) AND
				(
					$6::bool IS NULL OR
					$6::bool AND run.status = 'failed' OR
					NOT $6::bool AND (run.status != 'failed' OR run IS NULL)
				) AND
				(
					$7::bool IS NULL OR
					$7::bool AND run.status = 'canceled' OR
					NOT $7::bool AND (run.status != 'canceled' OR run IS NULL)
				) AND
				($8::uuid IS NULL OR invocation.action_id = $8::uuid)
		`, `invocation.created_at DESC`,
		opts.Private.Ptr(),
		opts.Filter.InvocationInvoking.Ptr(),
		opts.Filter.InvocationFailed.Ptr(),
		opts.Filter.RunRunning.Ptr(),
		opts.Filter.RunSucceeded.Ptr(),
		opts.Filter.RunFailed.Ptr(),
		opts.Filter.RunCanceled.Ptr(),
		opts.ActionId,
	)
}

// `ok`: Allows to filter for successful or failed invocations.
func (self *invocationRepository) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, ok util.MayBool, page *repository.Page) ([]domain.Invocation, error) {
	joins := ``
	for i := range factIds {
		iStr := strconv.Itoa(i + 1)
		joins += ` JOIN invocation_inputs AS invocation_inputs_` + iStr + ` ON
			invocation_inputs_` + iStr + `.invocation_id = invocation.id AND
			invocation_inputs_` + iStr + `.fact_id = $` + iStr
	}

	args := make([]any, len(factIds))
	for i, factId := range factIds {
		args[i] = factId
	}

	var whereOk string
	if okPtr := ok.Ptr(); okPtr == nil {
		whereOk = ``
	} else {
		whereOk = `WHERE `
		if !*okPtr {
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

	invocations := make([]domain.Invocation, page.Limit)
	return invocations, fetchPage(
		self.db, page, &invocations,
		`invocation.*`, from, `created_at`,
		args...,
	)
}

func (self *invocationRepository) Save(invocation *domain.Invocation, inputs map[string]domain.Fact) error {
	ctx := context.Background()

	if err := pgx.BeginFunc(ctx, self.db, func(tx pgx.Tx) error {
		if err := tx.QueryRow(
			ctx,
			`INSERT INTO invocation (action_id) VALUES ($1) RETURNING id, created_at`,
			invocation.ActionId,
		).Scan(&invocation.Id, &invocation.CreatedAt); err != nil {
			return err
		}

		if len(inputs) > 0 {
			sql := `INSERT INTO invocation_inputs (invocation_id, input_name, fact_id) VALUES`
			args := []any{}

			i := 1
			for name, fact := range inputs {
				if i > 1 {
					sql += `, `
				}

				sql += `($` + strconv.Itoa(i) + `, $` + strconv.Itoa(i+1) + `, $` + strconv.Itoa(i+2) + `)`
				args = append(args, invocation.Id, name, fact.ID)
				i += 3
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

func (self *invocationRepository) End(id uuid.UUID) (err error) {
	_, err = self.db.Exec(
		context.Background(),
		`UPDATE invocation SET finished_at = $2 WHERE id = $1`,
		id,
		// Do not use `STATEMENT_TIMESTAMP()` to avoid
		// issues when the DB time is even just slightly
		// different from Cicero's time.
		time.Now().UTC(),
	)
	return
}
