package config

import (
	"context"
	"errors"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgtype"
	pgtypeuuid "github.com/jackc/pgtype/ext/gofrs-uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type PgxIface interface {
	Query(context.Context, string, ...interface{}) (pgx.Rows, error)
	QueryRow(context.Context, string, ...interface{}) pgx.Row
	Exec(context.Context, string, ...interface{}) (pgconn.CommandTag, error)
	BeginFunc(context.Context, func(pgx.Tx) error) error
	SendBatch(context.Context, *pgx.Batch) pgx.BatchResults
}

var (
	_ PgxIface = &pgxpool.Pool{}
	_ PgxIface = &pgx.Conn{}
	_ PgxIface = pgx.Tx(nil)
)

func DBConnection() (PgxIface, error) {
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

	return pgxpool.ConnectConfig(context.Background(), dbconfig)
}
