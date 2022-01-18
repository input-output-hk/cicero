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

func (a *runRepository) GetByActionId(id uuid.UUID, fetchParam *domain.FetchParam) (runs []*domain.Run, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &runs,
		`SELECT * FROM run WHERE action_id = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3`,
		id,
		fetchParam.Limit+1,
		fetchParam.OffSet,
	)
	return
}

func (a *runRepository) GetAll(fetchParam *domain.FetchParam) (instances []*domain.Run, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &instances,
		`SELECT * FROM run ORDER BY created_at DESC LIMIT $1 OFFSET $2`,
		fetchParam.Limit+1,
		fetchParam.OffSet,
	)
	return
}

func (a *runRepository) Save(tx pgx.Tx, run *domain.Run) (err error) {
	err = tx.QueryRow(
		context.Background(),
		`INSERT INTO run (action_id) VALUES ($1) RETURNING nomad_job_id`,
		run.ActionId,
	).Scan(&run.NomadJobID)
	return
}

func (a *runRepository) Update(tx pgx.Tx, run *domain.Run) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`UPDATE run SET finished_at = $2 WHERE nomad_job_id = $1`,
		run.NomadJobID, run.FinishedAt,
	)
	return
}
