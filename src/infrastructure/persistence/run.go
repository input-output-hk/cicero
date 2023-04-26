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

func (a runRepository) Get(page *repository.Page, opts repository.RunGetOpts) ([]domain.Run, error) {
	var runs []domain.Run
	if page == nil {
		runs = make([]domain.Run, 0)
	} else {
		runs = make([]domain.Run, 0, page.Limit)
	}
	return runs, fetchPage(
		a.DB, page, &runs,
		`run.*`, `run
			INNER JOIN invocation  ON invocation.id    = run.invocation_id
			INNER JOIN action      ON action.id        = invocation.action_id
			INNER JOIN action_name ON action_name.name = action.name
			WHERE
				($1::bool IS NULL OR $1::bool = action_name.private) AND
				($2::bool IS NULL OR $2::bool = (run.finished_at IS NULL)) AND
				($3::uuid IS NULL OR $3::uuid = invocation.action_id)
		`, `run.created_at DESC`,
		opts.Private.Ptr(),
		opts.Finished.Ptr(),
		opts.ActionId,
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
