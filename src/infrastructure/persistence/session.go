package persistence

import (
	"context"
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/georgysavva/scany/v2/pgxscan"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type sessionRepository struct {
	Db config.PgxIface
}

func NewSessionRepository(db config.PgxIface) repository.SessionRepository {
	return &sessionRepository{db}
}

func (self *sessionRepository) WithQuerier(querier config.PgxIface) repository.SessionRepository {
	return &sessionRepository{querier}
}

func (self *sessionRepository) GetExpiredBy(expiry time.Time) ([]pgstore.PGSession, error) {
	sessions := []pgstore.PGSession{}
	return sessions, pgxscan.Select(
		context.Background(), self.Db, &sessions,
		`SELECT id, encode(key, 'escape') AS key, encode(data, 'escape') AS data, created_on, modified_on, expires_on FROM http_sessions WHERE expires_on <= $1`,
		expiry,
	)
}

func (self *sessionRepository) Update(session pgstore.PGSession) (err error) {
	_, err = self.Db.Exec(context.Background(), `
		UPDATE http_sessions SET
			data = $2,
			modified_on = $3,
			expires_on = $4
		WHERE id = $1
	`, session.ID, session.Data, session.ModifiedOn, session.ExpiresOn)
	return
}
