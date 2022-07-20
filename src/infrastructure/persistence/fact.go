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
	"github.com/jackc/pgx/v5"
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

func (a *factRepository) GetLatestByCue(value cue.Value) (*domain.Fact, error) {
	where, args := sqlWhereCue(value, nil, 0)
	fact, err := get(
		a.DB, &domain.Fact{},
		`SELECT id, run_id, value, created_at, binary_hash FROM fact WHERE `+where+` ORDER BY created_at DESC FETCH FIRST ROW ONLY`,
		args...,
	)
	if fact == nil {
		return nil, err
	}
	return fact.(*domain.Fact), err
}

func (a *factRepository) GetByCue(value cue.Value) (facts []domain.Fact, err error) {
	where, args := sqlWhereCue(value, nil, 0)
	facts = []domain.Fact{}
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
	}

	appendTextEquals := func(arg string) {
		clause = `jsonb_extract_path_text(`
		appendPath()
		argNum += 1
		clause += `) = $` + strconv.Itoa(argNum)
		args = append(args, arg)
	}

	appendArg := func(arg cue.Value) {
		var v interface{}
		var cast string

		switch arg.Kind() {
		case cue.BytesKind:
			v, _ = arg.Bytes()
			cast = "bytea"
		case cue.IntKind:
			v, _ = arg.Int64()
			cast = "integer"
		case cue.FloatKind:
			v, _ = arg.Float64()
			cast = "real"
		case cue.NumberKind:
			panic("we should handle all number kinds specifically")
		case cue.BoolKind:
			v, _ = arg.Bool()
			cast = "boolean"
		case cue.NullKind:
			v = "null"
			cast = "jsonb"
		case cue.StringKind:
			v, _ = arg.String()
			cast = "text"
		default:
			panic("arg must be concrete scalar")
		}

		argNum += 1
		clause += `$` + strconv.Itoa(argNum) + `::` + cast
		args = append(args, v)
	}

	appendComparision := func(cmp string, arg cue.Value) {
		clause = `jsonb_extract_path(`
		appendPath()
		clause += `) ` + cmp + ` to_jsonb(`
		appendArg(arg)
		clause += `)`
	}

	appendClause := func(subClause string, subArgs []interface{}) {
		clause += subClause
		args = append(args, subArgs...)
		argNum += len(subArgs)
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

Kind:
	switch value.Kind() {
	case cue.StructKind:
		emptyStruct := true
		for iter, _ := value.Fields(); iter.Next(); {
			selector := iter.Selector()

			if iter.IsOptional() || selector.IsDefinition() || selector.PkgPath() != "" || !selector.IsString() {
				continue
			}

			emptyStruct = false

			if fieldClause, fieldArgs := sqlWhereCue(iter.Value(), append(path, iter.Label()), argNum); fieldClause != "" {
				appendClause(and()+fieldClause, fieldArgs)
			}
		}
		if emptyStruct {
			appendClause(and()+"TRUE", []interface{}{})
		}
	case cue.ListKind:
		list, _ := value.List()
		i := 0
		for ; list.Next(); i += 1 {
			itemClause, itemArgs := sqlWhereCue(list.Value(), append(path, strconv.Itoa(i)), argNum)
			appendClause(and()+itemClause, itemArgs)
		}
		if i == 0 {
			appendClause(and()+"TRUE", []interface{}{})
		}
	case cue.StringKind:
		str, _ := value.String()
		appendTextEquals(str)
	case cue.BytesKind, cue.IntKind, cue.FloatKind, cue.NumberKind, cue.BoolKind, cue.NullKind:
		appendComparision("=", value)
	case cue.BottomKind, cue.TopKind:
		switch op, vals := value.Expr(); op {
		case cue.OrOp:
			clause = `(`
			for i, val := range vals {
				if i > 0 {
					clause += ` OR `
				}
				appendClause(sqlWhereCue(val, path, argNum))
			}
			clause += `)`
			break Kind
		case cue.AndOp:
			clause = `(`
			for i, val := range vals {
				if i > 0 {
					clause += ` AND `
				}
				appendClause(sqlWhereCue(val, path, argNum))
			}
			clause += `)`
			break Kind
		case cue.GreaterThanOp:
			if val := vals[0]; val.IsConcrete() {
				appendComparision(">", val)
				break Kind
			}
		case cue.LessThanOp:
			if val := vals[0]; val.IsConcrete() {
				appendComparision("<", val)
				break Kind
			}
		case cue.LessThanEqualOp:
			if val := vals[0]; val.IsConcrete() {
				appendComparision("<=", val)
				break Kind
			}
		case cue.GreaterThanEqualOp:
			if val := vals[0]; val.IsConcrete() {
				appendComparision(">=", val)
				break Kind
			}
		case cue.NotEqualOp:
			if val := vals[0]; val.IsConcrete() {
				appendComparision("<>", val)
				break Kind
			}
		}

		clause = `jsonb_extract_path(`
		appendPath()
		clause += `) IS NOT NULL`
	default:
		panic("switch should be exhaustive")
	}

	return
}

func (a *factRepository) Save(fact *domain.Fact, binary io.Reader) error {
	ctx := context.Background()
	return pgx.BeginFunc(ctx, a.DB, func(tx pgx.Tx) error {
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
