package persistence

import (
	"context"
	"encoding/json"
	"fmt"
	"io"

	"github.com/direnv/direnv/v2/sri"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type factRepository struct {
	DB config.PgxIface
}

func NewFactRepository(db config.PgxIface) repository.FactRepository {
	return &factRepository{db}
}

func (a *factRepository) WithQuerier(querier config.PgxIface) repository.FactRepository {
	return &factRepository{querier}
}

func (a *factRepository) GetById(id uuid.UUID) (*domain.Fact, error) {
	fact, err := get(
		a.DB, &domain.Fact{},
		`SELECT id, run_id, value, created_at, binary_hash FROM fact WHERE id = $1`,
		id,
	)
	if fact == nil {
		return nil, err
	}
	return fact.(*domain.Fact), err
}

func (a *factRepository) GetByIds(idMap map[string]uuid.UUID) (facts map[string]domain.Fact, err error) {
	ids := make([]uuid.UUID, 0, len(idMap))
	for _, v := range idMap {
		ids = append(ids, v)
	}

	result := []domain.Fact{}
	err = pgxscan.Select(
		context.Background(), a.DB, &result,
		`SELECT id, run_id, value, created_at, binary_hash
		FROM fact
		WHERE id = ANY($1)`,
		ids,
	)
	if err != nil {
		return
	}

	facts = map[string]domain.Fact{}
Result:
	for _, fact := range result {
		for k, v := range idMap {
			if v == fact.ID {
				facts[k] = fact
				continue Result
			}
		}
		panic("This should never happen™")
	}

	return
}

func (a *factRepository) GetByRunId(id uuid.UUID) (facts []domain.Fact, err error) {
	facts = []domain.Fact{}
	err = pgxscan.Select(
		context.Background(), a.DB, &facts,
		`SELECT id, run_id, value, created_at, binary_hash
		FROM fact WHERE run_id = $1
		ORDER BY created_at DESC`,
		id,
	)
	return
}

func (a *factRepository) GetBinaryById(tx pgx.Tx, id uuid.UUID) (binary io.ReadSeekCloser, err error) {
	var oid uint32
	err = pgxscan.Get(
		context.Background(), tx, &oid,
		`SELECT "binary" FROM fact WHERE id = $1`,
		id,
	)
	if err != nil {
		return
	}

	los := tx.LargeObjects()
	binary, err = los.Open(context.Background(), oid, pgx.LargeObjectModeRead)
	err = errors.WithMessagef(err, "Failed to open large object with OID %d", oid)

	return
}

func (a *factRepository) Save(fact *domain.Fact, binary io.Reader) error {
	ctx := context.Background()
	return a.DB.BeginFunc(ctx, func(tx pgx.Tx) error {
		var binaryOid *uint32
		if binary != nil {
			los := tx.LargeObjects()
			if oid, err := los.Create(ctx, 0); err != nil {
				return errors.WithMessage(err, "Failed to create large object")
			} else if lo, err := los.Open(ctx, oid, pgx.LargeObjectModeWrite); err != nil {
				return errors.WithMessagef(err, "Failed to open large object with OID %d", oid)
			} else {
				hash := sri.NewWriter(lo, sri.SHA256)
				switch written, err := io.Copy(hash, binary); {
				case err != nil:
					return errors.WithMessagef(err, "Failed to write to large object with OID %d", oid)
				case written == 0:
					// Nothing was written because the read stream was empty.
					// We treat that case as if we had `binary == nil`.
					if err := los.Unlink(ctx, oid); err != nil {
						return errors.WithMessagef(err, "Failed to unlink large object with OID %d", oid)
					}
				default:
					sum := hash.Sum()
					if fact.BinaryHash != nil && *fact.BinaryHash != sum {
						return fmt.Errorf("Binary has hash %q instead of expected %q", sum, *fact.BinaryHash)
					}
					fact.BinaryHash = &sum
					binaryOid = &oid
				}
			}
		}

		// You might think "why not just pass `fact.Value` to `pgxscan.Get()` directly?"
		// but that has other marshalling rules than `json.Marshal()`.
		// Most importantly it would pass strings and null verbatim to postgres.
		factJson, err := json.Marshal(fact.Value)
		if err != nil {
			return errors.WithMessage(err, "Could not marshal fact to JSON")
		}

		return pgxscan.Get(
			ctx, tx, fact,
			`INSERT INTO fact (run_id, value, binary_hash, "binary") VALUES ($1, $2, $3, $4) RETURNING id, created_at`,
			fact.RunId, factJson, fact.BinaryHash, binaryOid,
		)
	})
}
