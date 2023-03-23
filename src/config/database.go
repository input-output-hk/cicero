package config

import (
	"context"
	"errors"

	pgxzerolog "github.com/jackc/pgx-zerolog"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgconn"
	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/jackc/pgx/v5/tracelog"
	"github.com/rs/zerolog"
	pgxuuid "github.com/vgarvardt/pgx-google-uuid/v5"
)

type PgxIface interface {
	Query(context.Context, string, ...any) (pgx.Rows, error)
	QueryRow(context.Context, string, ...any) pgx.Row
	Exec(context.Context, string, ...any) (pgconn.CommandTag, error)
	SendBatch(context.Context, *pgx.Batch) pgx.BatchResults
	Begin(context.Context) (pgx.Tx, error)
}

var (
	_ PgxIface = &pgxpool.Pool{}
	_ PgxIface = &pgx.Conn{}
	_ PgxIface = pgx.Tx(nil)
)

func DbUrl() (url string, err error) {
	url = GetenvStr("DATABASE_URL")
	if url == "" {
		err = errors.New("Environment variable DATABASE_URL not set or empty")
	}
	return
}

func DBConnection(logger *zerolog.Logger, logDb bool) (*pgxpool.Pool, error) {
	url, err := DbUrl()
	if err != nil {
		return nil, err
	}

	dbconfig, err := pgxpool.ParseConfig(url)
	if err != nil {
		return nil, err
	}
	if logDb {
		dbconfig.ConnConfig.Tracer = &tracelog.TraceLog{
			Logger:   pgxzerolog.NewLogger(*logger),
			LogLevel: tracelog.LogLevelTrace,
		}
	}

	//TODO: log configuration
	dbconfig.AfterConnect = func(ctx context.Context, conn *pgx.Conn) error {
		pgxuuid.Register(conn.TypeMap())
		return nil
	}

	return pgxpool.NewWithConfig(context.Background(), dbconfig)
}
