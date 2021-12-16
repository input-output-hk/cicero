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

func (a *factRepository) GetLatestByFields(fields []string) (fact domain.Fact, err error) {
	// FIXME query selects from MAX(created_at) over ALL facts, not only filtered. select from subquery or use GROUP BY.
	err = pgxscan.Get(
		context.Background(), a.DB, &fact,
		`SELECT * FROM facts WHERE ` + buildSQLWhereConditions(fields) + ` GROUP BY created_at HAVING created_at = MAX(created_at)`,
		fields,
	)
	return
}

func (a *factRepository) GetByFields(fields []string) (facts []*domain.Fact, err error) {
	err = pgxscan.Select(
		context.Background(), a.DB, &facts,
		`SELECT * FROM facts WHERE ` + buildSQLWhereConditions(fields),
		fields,
	)
	return
}

func buildSQLWhereConditions(paths []string) (where string) {
	// FIXME checks only against top-level field. Check PATH instead!
	for i := range paths {
		i += 1
		if i > 1 {
			where += " AND "
		}
		where += `value ?& $` + strconv.Itoa(i)
	}
	return
}

func (a *factRepository) Save(tx pgx.Tx, fact *domain.Fact) error {
	return tx.QueryRow(
		context.Background(),
		`INSERT INTO facts (run_id, value, binary_hash) VALUES ($1, $2, $3) RETURNING id`,
		fact.RunId, fact.Value, fact.BinaryHash,
	).Scan(&fact.ID)
	// TODO nyi: stream-insert binary using large object API. or do that in a separate function.
	// TODO nyi: unique key over (value, binary_hash)
}
