package config

import (
	"context"
	"errors"

	"github.com/jackc/pgx/v5/pgconn"
	// "github.com/jackc/pgx/v5/pgtype"
	pgxUUID "github.com/vgarvardt/pgx-google-uuid/v5"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/rs/zerolog"
)

type PgxIface interface {
	Query(context.Context, string, ...interface{}) (pgx.Rows, error)
	QueryRow(context.Context, string, ...interface{}) pgx.Row
	Exec(context.Context, string, ...interface{}) (pgconn.CommandTag, error)
	Begin(context.Context) (pgx.Tx, error)
	SendBatch(context.Context, *pgx.Batch) pgx.BatchResults
}

var (
	_ PgxIface = &pgxpool.Pool{}
	_ PgxIface = &pgx.Conn{}
	_ PgxIface = pgx.Tx(nil)
)

func DBConnection(logger *zerolog.Logger, logDb bool) (PgxIface, error) {
	url := GetenvStr("DATABASE_URL")
	if url == "" {
		return nil, errors.New("Environment variable DATABASE_URL not set or empty")
	}

	dbconfig, err := pgxpool.ParseConfig(url)
	if err != nil {
		return nil, err
	}
	if logDb {
		// TODO fix logging using https://github.com/jackc/pgx-zerolog
		// dbconfig.ConnConfig.Logger = wrapLogger(logger)
	}

	//TODO: log configuration
	dbconfig.AfterConnect = func(ctx context.Context, conn *pgx.Conn) error {
		pgxUUID.Register(conn.TypeMap())
		return nil
	}

	return pgxpool.NewConfig(context.Background(), dbconfig)
}

func wrapLogger(original *zerolog.Logger) pgLogger {
	return pgLogger{original}
}

type pgLogger struct{ original *zerolog.Logger }

func (l pgLogger) Log(ctx context.Context, level pgx.LogLevel, msg string, data map[string]interface{}) {
	var event *zerolog.Event
	switch level {
	case pgx.LogLevelTrace:
		event = l.original.Trace()
	case pgx.LogLevelDebug:
		event = l.original.Debug()
	case pgx.LogLevelInfo:
		event = l.original.Info()
	case pgx.LogLevelWarn:
		event = l.original.Warn()
	case pgx.LogLevelError:
		event = l.original.Error()
	}

	event = event.Str("lib", "pgx")
	for k, v := range data {
		event.Interface(k, v)
	}

	event.Msg(msg)
}
