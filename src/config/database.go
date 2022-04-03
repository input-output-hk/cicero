package config

import (
	"context"
	"errors"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgtype"
	pgtypeuuid "github.com/jackc/pgtype/ext/gofrs-uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/rs/zerolog"
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

func DBConnection(logger *zerolog.Logger) (PgxIface, error) {
	url := GetenvStr("DATABASE_URL")
	if url == "" {
		return nil, errors.New("Environment variable DATABASE_URL not set or empty")
	}

	dbconfig, err := pgxpool.ParseConfig(url)
	if err != nil {
		return nil, err
	}
	dbconfig.ConnConfig.Logger = wrapLogger(logger)

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

	for k, v := range data {
		event.Interface(k, v)
	}

	event.Msg(msg)
}
