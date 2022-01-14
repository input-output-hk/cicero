package mocks

import (
	"context"
	"errors"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/jackc/pgx/v4"
	"github.com/pashagolub/pgxmock"
	"testing"
)

type PgxConnIfaceMocked interface {
	pgxmock.PgxConnIface
	config.DBErrorHandler
}

type PgxIfaceMockedImpl struct {
	DB pgxmock.PgxConnIface
}

func NewDBMocked(db pgxmock.PgxConnIface) *PgxIfaceMockedImpl {
	return &PgxIfaceMockedImpl{DB: db}
}

func (self *PgxIfaceMockedImpl) Query(ctx context.Context, query string, args ...interface{}) (pgx.Rows, error) {
	return self.DB.Query(ctx, query, args)
}

func (self *PgxIfaceMockedImpl) BeginFunc(ctx context.Context, f func(pgx.Tx) error) error {
	return self.DB.BeginFunc(ctx, f)
}

func (self *PgxIfaceMockedImpl) NotFound(err error) bool {
	return errors.Is(err, pgx.ErrNoRows)
}

func BuildTransaction(ctx context.Context, t *testing.T) (*PgxIfaceMockedImpl, pgx.Tx) {
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
	return NewDBMocked(db), tx
}
