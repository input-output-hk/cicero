package persistence

import (
	"context"

	"github.com/google/uuid"

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

func (a runRepository) WithQuerier(querier config.PgxIface) repository.RunRepository {
	return &runRepository{querier}
}

func (a runRepository) GetByNomadJobId(id uuid.UUID) (*domain.Run, error) {
	return a.GetByNomadJobIdWithLock(id, "")
}

func (a runRepository) GetByNomadJobIdWithLock(id uuid.UUID, lock string) (*domain.Run, error) {
	run, err := get(
		a.DB, &domain.Run{},
		`SELECT * FROM run WHERE nomad_job_id = $1 `+lock,
		id,
	)
	if run == nil {
		return nil, err
	}
	return run.(*domain.Run), err
}

func (a runRepository) GetByInvocationId(invocationId uuid.UUID) (*domain.Run, error) {
	run, err := get(
		a.DB, &domain.Run{},
		`SELECT * FROM run WHERE invocation_id = $1`,
		invocationId,
	)
	if run == nil {
		return nil, err
	}
	return run.(*domain.Run), err
}

func (a runRepository) GetByActionId(id uuid.UUID, page *repository.Page) ([]domain.Run, error) {
	runs := make([]domain.Run, page.Limit)
	return runs, fetchPage(
		a.DB, page, &runs,
		`run.*`,
		`run JOIN invocation i ON i.id = run.invocation_id AND i.action_id = $1`,
		`created_at DESC`,
		id,
	)
}

func (a runRepository) GetLatestByActionId(id uuid.UUID) (*domain.Run, error) {
	run, err := get(
		a.DB, &domain.Run{},
		`SELECT DISTINCT ON (invocation.action_id) run.*
		FROM run
		JOIN invocation ON
			invocation.id = invocation_id AND
			invocation.action_id = $1
		ORDER BY invocation.action_id, run.created_at DESC`,
		id,
	)
	if run == nil {
		return nil, err
	}
	return run.(*domain.Run), err
}

func (a runRepository) GetAll(page *repository.Page) ([]domain.Run, error) {
	runs := make([]domain.Run, page.Limit)
	return runs, fetchPage(
		a.DB, page, &runs,
		`*`, `run`, `created_at DESC`,
	)
}

func (a runRepository) Save(run *domain.Run) error {
	return a.DB.QueryRow(
		context.Background(),
		`INSERT INTO run (invocation_id, status) VALUES ($1, $2) RETURNING nomad_job_id, created_at`,
		run.InvocationId, run.Status.String(),
	).Scan(&run.NomadJobID, &run.CreatedAt)
}

func (a runRepository) Update(run *domain.Run) (err error) {
	_, err = a.DB.Exec(
		context.Background(),
		`UPDATE run SET finished_at = $2, status = $3 WHERE nomad_job_id = $1`,
		run.NomadJobID, run.FinishedAt, run.Status.String(),
	)
	return
}
