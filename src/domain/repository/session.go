package repository

import (
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/input-output-hk/cicero/src/config"
)

type SessionRepository interface {
	WithQuerier(config.PgxIface) SessionRepository

	GetByKey(string) (*pgstore.PGSession, error)
	GetExpiredByAndZeroModified(time.Time) ([]pgstore.PGSession, error)
	Update(pgstore.PGSession) error
}
