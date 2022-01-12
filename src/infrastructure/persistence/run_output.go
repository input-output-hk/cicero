package persistence

import (
	"context"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type runOutputRepository struct {
	DB config.PgxIface
}

func NewRunOutputRepository(db config.PgxIface) repository.RunOutputRepository {
	return runOutputRepository{DB: db}
}

func (a runOutputRepository) GetByRunId(id uuid.UUID) (output domain.RunOutput, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &output,
		`SELECT success, failure FROM run_outputs WHERE run_id = $1`,
		id,
	)
	return
}

func (a runOutputRepository) Save(tx pgx.Tx, runId uuid.UUID, output *domain.RunOutput) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`INSERT INTO run_outputs (run_id, success, failure) VALUES ($1, $2, $3)`,
		runId, output.Success, output.Failure,
	)
	return
}

func (a runOutputRepository) Update(tx pgx.Tx, runId uuid.UUID, output *domain.RunOutput) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE run_outputs (success, failure) VALUES ($2, $3) WHERE run_id = $1`,
		runId, output.Success, output.Failure,
	)
	return
}

func (a runOutputRepository) Delete(tx pgx.Tx, runId uuid.UUID) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`DELETE FROM run_outputs WHERE run_id = $1`,
		runId,
	)
	return
}
