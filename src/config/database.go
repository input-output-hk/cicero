package config

import (
	"context"
	"errors"

	"github.com/jackc/pgtype"
	pgtypeuuid "github.com/jackc/pgtype/ext/gofrs-uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type DBInterface interface {
	Query(ctx context.Context, query string, args ...interface{}) (pgx.Rows, error)
	BeginFunc(context.Context, func(pgx.Tx) error) error
}

type DBErrorHandler interface {
	NotFound(err error) bool
}

//This interface help us to mock the database interface, decoupling the queries for the error handler
type PgxIface interface {
	DBInterface
	DBErrorHandler
}

type PgxIfaceImpl struct {
	pool DBInterface
}

func (self *PgxIfaceImpl) Query(ctx context.Context, query string, args ...interface{}) (pgx.Rows, error) {
	return self.pool.Query(ctx, query, args)
}

func (self *PgxIfaceImpl) BeginFunc(ctx context.Context, f func(pgx.Tx) error) error {
	return self.pool.BeginFunc(ctx, f)
}

func (self *PgxIfaceImpl) NotFound(err error) bool {
	return errors.Is(err, pgx.ErrNoRows)
}

func DBConnection() (*PgxIfaceImpl, error) {
	url := GetenvStr("DATABASE_URL")
	if url == "" {
		return nil, errors.New("Environment variable DATABASE_URL not set or empty")
	}

	dbconfig, err := pgxpool.ParseConfig(url)
	if err != nil {
		return nil, err
	}

	//TODO: log configuration
	dbconfig.AfterConnect = func(ctx context.Context, conn *pgx.Conn) error {
		conn.ConnInfo().RegisterDataType(pgtype.DataType{
			Value: &pgtypeuuid.UUID{},
			Name:  "uuid",
			OID:   pgtype.UUIDOID,
		})
		return nil
	}

	pool, err := pgxpool.ConnectConfig(context.Background(), dbconfig)
	if err != nil {
		return nil, err
	}
	return &PgxIfaceImpl{pool: pool}, nil
}
