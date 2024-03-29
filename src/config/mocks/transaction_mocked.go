package mocks

import (
	"context"
	"github.com/jackc/pgx/v5"
	"github.com/pashagolub/pgxmock/v2"
	"testing"
)

func BuildTransaction(ctx context.Context, t *testing.T) (pgxmock.PgxConnIface, pgx.Tx) {
	db, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err.Error())
	}
	defer db.Close(ctx)
	db.ExpectBegin()
	tx, err := db.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err.Error())
	}
	defer func() { _ = tx.Rollback(ctx) }()
	return db, tx
}
