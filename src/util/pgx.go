package util

import (
	"errors"
	"github.com/jackc/pgx/v5"
)

func ScanNextRow(rows pgx.Rows, dst ...any) error {
	defer rows.Close()
	if err := rows.Err(); err != nil {
		return err
	} else if !rows.Next() {
		return errors.New("no row")
	} else if err := rows.Scan(dst...); err != nil {
		return err
	}
	return nil
}
