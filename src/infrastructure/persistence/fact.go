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

type factRepository struct {
	DB config.PgxIface
}

func NewFactRepository(db config.PgxIface) repository.FactRepository {
	return &factRepository{DB: db}
}

func (a *factRepository) GetById(id uuid.UUID) (fact domain.Fact, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &fact,
		`SELECT * FROM facts WHERE id = $1`,
		id,
	)
	return
}

func (a *factRepository) GetLatestByFields(fields [][]string) (fact domain.Fact, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &fact,
		`SELECT * FROM facts `+sqlWhereHasPaths(fields)+` ORDER BY created_at DESC FETCH FIRST ROW ONLY`,
		pathsToQueryArgs(fields)...,
	)
	return
}

func (a *factRepository) GetByFields(fields [][]string) (facts []*domain.Fact, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &facts,
		`SELECT * FROM facts `+sqlWhereHasPaths(fields),
		pathsToQueryArgs(fields)...,
	)
	return
}

func sqlWhereHasPaths(paths [][]string) (where string) {
	if len(paths) == 0 {
		return
	}

	where += ` WHERE `
	n := 1
	for i, path := range paths {
		if i > 0 {
			where += ` AND `
		}
		where += ` jsonb_extract_path(value `
		for range path {
			where += ` , $` + strconv.Itoa(n)
			n += 1
		}
		where += ` ) IS NOT NULL `
	}

	return
}

func pathsToQueryArgs(paths [][]string) (args []interface{}) {
	for _, path := range paths {
		for _, field := range path {
			args = append(args, field)
		}
	}
	return
}

func (a *factRepository) Save(tx pgx.Tx, fact *domain.Fact) error {
	return pgxscan.Get(
		context.Background(), tx, fact,
		`INSERT INTO facts (run_id, value, binary_hash) VALUES ($1, $2, $3) RETURNING id, created_at`,
		fact.RunId, fact.Value, fact.BinaryHash,
	)
	// TODO nyi: stream-insert binary using large object API. or do that in a separate function.
	// TODO nyi: unique key over (value, binary_hash)
}
