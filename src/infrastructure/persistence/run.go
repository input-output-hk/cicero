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
	return &runRepository{DB: db}
}

func (a *runRepository) GetByNomadJobId(id uuid.UUID) (run domain.Run, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &run,
		`SELECT * FROM run WHERE nomad_job_id = $1`,
		id,
	)
	return
}

func (a *runRepository) GetByActionId(id uuid.UUID, limit int, offSet int) (runs []*domain.Run, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &runs,
		`SELECT * FROM run WHERE action_id = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3`,
		id,
		limit,
		offSet,
	)
	return
}

func (a *runRepository) GetLatestByActionId(tx pgx.Tx, id uuid.UUID) (run domain.Run, err error) {
	err = pgxscan.Get(
		context.Background(), tx, &run,
		`SELECT DISTINCT ON (action_id) * FROM run WHERE action_id = $1 ORDER BY action_id, created_at DESC`,
		id,
	)
	return
}

func (a *runRepository) GetInputFactIdsByNomadJobId(tx pgx.Tx, id uuid.UUID) (inputFactIds map[string][]uuid.UUID, err error) {
	runInputs := []struct {
		InputName string    `json:"input_name"`
		FactId    uuid.UUID `json:"fact_id"`
	}{}

	err = pgxscan.Select(
		context.Background(), tx, &runInputs,
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

func (a *runRepository) GetAll(limit int, offSet int) (instances []*domain.Run, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &instances,
		`SELECT * FROM run ORDER BY created_at DESC LIMIT $1 OFFSET $2`,
		limit,
		offSet,
	)
	return
}

func (a *runRepository) Save(tx pgx.Tx, run *domain.Run, inputs map[string]interface{}) error {
	ctx := context.Background()

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
}

func (a *runRepository) Update(tx pgx.Tx, run *domain.Run) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE run SET finished_at = $2 WHERE nomad_job_id = $1`,
		run.NomadJobID, run.FinishedAt,
	)
	return
}
