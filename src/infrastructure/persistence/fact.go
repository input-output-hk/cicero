package persistence

import (
	"context"
	"fmt"
	"io"
	"strconv"

	"cuelang.org/go/cue"
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

func (a *factRepository) GetById(id uuid.UUID) (fact domain.Fact, err error) {
	err = pgxscan.Get(
		context.Background(), a.DB, &fact,
		`SELECT id, run_id, value, created_at, binary_hash FROM fact WHERE id = $1`,
		id,
	)
	return
}

func (a *factRepository) GetByRunId(id uuid.UUID) (facts []*domain.Fact, err error) {
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

func (a *factRepository) GetLatestByCue(value cue.Value) (fact domain.Fact, err error) {
	where, args := sqlWhereCue(value, []string{}, 0)
	err = pgxscan.Get(
		context.Background(), a.DB, &fact,
		`SELECT id, run_id, value, created_at, binary_hash FROM fact WHERE `+where+` ORDER BY created_at DESC FETCH FIRST ROW ONLY`,
		args...,
	)
	return
}

func (a *factRepository) GetByCue(value cue.Value) (facts []*domain.Fact, err error) {
	where, args := sqlWhereCue(value, []string{}, 0)
	err = pgxscan.Select(
		context.Background(), a.DB, &facts,
		`SELECT id, run_id, value, created_at, binary_hash FROM fact WHERE `+where,
		args...,
	)
	return
}

func sqlWhereCue(value cue.Value, path []string, argNum int) (clause string, args []interface{}) {
	appendPath := func() {
		clause += `value`
		for _, part := range path {
			argNum += 1
			clause += `, $` + strconv.Itoa(argNum)
			args = append(args, part)
		}
		return
	}

	appendTextEquals := func(arg string) {
		clause = `jsonb_extract_path_text(`
		appendPath()
		argNum += 1
		clause += `) = $` + strconv.Itoa(argNum)
		args = append(args, arg)
	}

	appendEquals := func(arg interface{}, cast string) {
		clause = `jsonb_extract_path(`
		appendPath()
		argNum += 1
		clause += `) = to_jsonb($` + strconv.Itoa(argNum) + `::` + cast + `)`
		args = append(args, arg)
	}

	var and func() string
	{
		first := true
		and = func() string {
			if first {
				first = false
				return ``
			}
			return ` AND `
		}
	}

	switch value.Kind() {
	case cue.StructKind:
		strukt, _ := value.Struct()
		iter := strukt.Fields()
		for iter.Next() {
			selector := iter.Selector()

			if iter.IsOptional() || selector.IsDefinition() || selector.PkgPath() != "" || !selector.IsString() {
				continue
			}

			fieldClause, fieldArgs := sqlWhereCue(iter.Value(), append(path, iter.Label()), argNum)
			clause += and() + fieldClause
			args = append(args, fieldArgs...)
			argNum += len(fieldArgs)
		}
	case cue.ListKind:
		list, _ := value.List()
		i := 0
		for list.Next() {
			itemClause, itemArgs := sqlWhereCue(list.Value(), append(path, strconv.Itoa(i)), argNum)
			i += 1

			clause += and() + itemClause
			args = append(args, itemArgs...)
			argNum += len(itemArgs)
		}
	case cue.StringKind:
		str, _ := value.String()
		appendTextEquals(str)
	case cue.BytesKind:
		bytes, _ := value.Bytes()
		appendEquals(bytes, "bytea")
	case cue.IntKind:
		num, _ := value.Int64()
		appendEquals(num, "integer")
	case cue.FloatKind:
		num, _ := value.Float64()
		appendEquals(num, "real")
	case cue.NumberKind:
		panic("we should handle all number kinds specifically")
	case cue.BoolKind:
		b, _ := value.Bool()
		appendEquals(b, "boolean")
	case cue.NullKind:
		appendEquals("null", "jsonb")
	default:
		clause = `jsonb_extract_path(`
		appendPath()
		clause += `) IS NOT NULL`
	}

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

		return pgxscan.Get(
			ctx, tx, fact,
			`INSERT INTO fact (run_id, value, binary_hash, "binary") VALUES ($1, $2, $3, $4) RETURNING id, created_at`,
			fact.RunId, fact.Value, fact.BinaryHash, binaryOid,
		)
	})
}
