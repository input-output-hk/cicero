package config

import (
	"context"
	"errors"

	"github.com/jackc/pgtype"
	pgtypeuuid "github.com/jackc/pgtype/ext/gofrs-uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type PgxIface interface {
	Query(ctx context.Context, query string, args ...interface{}) (pgx.Rows, error)
	BeginFunc(context.Context, func(pgx.Tx) error) error
	SendBatch(context.Context, *pgx.Batch) pgx.BatchResults
}

func DBConnection() (*pgxpool.Pool, error) {
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
