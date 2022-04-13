package persistence

import (
	"context"
	"strconv"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type runRepository struct {
	DB config.PgxIface
}

func NewRunRepository(db config.PgxIface) repository.RunRepository {
	return &runRepository{db}
}

func (a *runRepository) WithQuerier(querier config.PgxIface) repository.RunRepository {
	return &runRepository{querier}
}

func (a *runRepository) GetByNomadJobId(id uuid.UUID) (domain.Run, error) {
	return a.GetByNomadJobIdWithLock(id, "")
}

func (a *runRepository) GetByNomadJobIdWithLock(id uuid.UUID, lock string) (run domain.Run, err error) {
	return run, pgxscan.Get(
		context.Background(), a.DB, &run,
		`SELECT * FROM run WHERE nomad_job_id = $1 `+lock,
		id,
	)
}

func (a *runRepository) GetByActionId(id uuid.UUID, page *repository.Page) ([]*domain.Run, error) {
	runs := make([]*domain.Run, page.Limit)
	return runs, fetchPage(
		a.DB, page, &runs,
		`*`, `run WHERE action_id = $1`, `created_at DESC`,
		id,
	)
}

func (a *runRepository) GetLatestByActionId(id uuid.UUID) (run domain.Run, err error) {
	return run, pgxscan.Get(
		context.Background(), a.DB, &run,
		`SELECT DISTINCT ON (action_id) * FROM run WHERE action_id = $1 ORDER BY action_id, created_at DESC`,
		id,
	)
}

func (a *runRepository) GetInputFactIdsByNomadJobId(id uuid.UUID) (inputFactIds repository.RunInputFactIds, err error) {
	runInputs := []struct {
		InputName string    `json:"input_name"`
		FactId    uuid.UUID `json:"fact_id"`
	}{}

	err = pgxscan.Select(
		context.Background(), a.DB, &runInputs,
		`SELECT input_name, fact_id FROM run_inputs WHERE run_id = $1`,
		id,
	)
	if err != nil {
		return
	}

	inputFactIds = map[string][]uuid.UUID{}
	for _, runInput := range runInputs {
		if _, exists := inputFactIds[runInput.InputName]; !exists {
			inputFactIds[runInput.InputName] = []uuid.UUID{}
		}
		inputFactIds[runInput.InputName] = append(inputFactIds[runInput.InputName], runInput.FactId)
	}

	return
}

func (a *runRepository) GetAll(page *repository.Page) ([]*domain.Run, error) {
	runs := make([]*domain.Run, page.Limit)
	return runs, fetchPage(
		a.DB, page, &runs,
		`*`, `run`, `created_at DESC`,
	)
}

func (a *runRepository) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, page *repository.Page) ([]*domain.Run, error) {
	joins := ``
	for i := range factIds {
		iStr := strconv.Itoa(i + 1)
		joins += ` JOIN run_inputs AS run_inputs_` + iStr + ` ON
			run_inputs_` + iStr + `.run_id = run.nomad_job_id AND
			run_inputs_` + iStr + `.fact_id = $` + iStr
	}

	args := make([]interface{}, len(factIds))
	for i, factId := range factIds {
		args[i] = factId
	}

	var from string
	if recursive {
		from = `(
			WITH RECURSIVE runs AS (
				SELECT run.*
				FROM run
				` + joins + `

				UNION

				SELECT run.*
				FROM run
				JOIN runs ON
					EXISTS (
						SELECT NULL
						FROM run_inputs
						JOIN fact ON
							fact.id = run_inputs.fact_id AND
							fact.run_id = runs.nomad_job_id
						WHERE run_inputs.run_id = run.nomad_job_id
					)
			) SELECT * FROM runs
		) AS run`
	} else {
		from = `run ` + joins
	}

	runs := make([]*domain.Run, page.Limit)
	return runs, fetchPage(
		a.DB, page, &runs,
		`run.*`, from, `created_at`,
		args...,
	)
}

func (a *runRepository) Save(run *domain.Run, inputs map[string]interface{}) error {
	ctx := context.Background()

	if err := a.DB.BeginFunc(ctx, func(tx pgx.Tx) error {
		if err := tx.QueryRow(
			ctx,
			`INSERT INTO run (action_id) VALUES ($1) RETURNING nomad_job_id, created_at`,
			run.ActionId,
		).Scan(&run.NomadJobID, &run.CreatedAt); err != nil {
			return err
		}

		if len(inputs) > 0 {
			sql := `INSERT INTO run_inputs (run_id, input_name, fact_id) VALUES`
			args := []interface{}{}

			i := 1
			for name, factOrFacts := range inputs {
				addRow := func(factId uuid.UUID) {
					if i > 1 {
						sql += `, `
					}

					sql += `($` + strconv.Itoa(i) + `, $` + strconv.Itoa(i+1) + `, $` + strconv.Itoa(i+2) + `)`
					args = append(args, run.NomadJobID, name, factId)
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

func (a *runRepository) Update(run *domain.Run) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`UPDATE run SET finished_at = $2 WHERE nomad_job_id = $1`,
		run.NomadJobID, run.FinishedAt,
	)
	return
}
