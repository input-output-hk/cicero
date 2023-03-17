package repository

import (
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/input-output-hk/cicero/src/config"
)

type SessionRepository interface {
	WithQuerier(config.PgxIface) SessionRepository

	GetExpiredBy(time.Time) ([]pgstore.PGSession, error)
	Update(pgstore.PGSession) error
}
