package model

import (
	"database/sql"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
	"github.com/uptrace/bun/dialect/sqlitedialect"
	"github.com/uptrace/bun/driver/sqliteshim"
)

func DBConnection() (*bun.DB, error) {
	sqldb, err := sql.Open(sqliteshim.ShimName, "db/database.sqlite3")
	if err != nil {
		return nil, errors.WithMessage(err, "While opening the DB")
	}

	db := bun.NewDB(sqldb, sqlitedialect.New())

	return db, nil
}
