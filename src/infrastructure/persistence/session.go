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

func (self *sessionRepository) GetByKey(key string) (*pgstore.PGSession, error) {
	session, err := get(
		self.Db, &pgstore.PGSession{},
		`SELECT id, encode(key, 'escape') AS key, encode(data, 'escape') AS data, created_on, modified_on, expires_on
		FROM http_sessions
		WHERE key = $1`,
		key,
	)
	if session == nil {
		return nil, err
	}
	return session.(*pgstore.PGSession), err
}

func (self *sessionRepository) GetExpiredByAndZeroModified(expiry time.Time) ([]pgstore.PGSession, error) {
	sessions := []pgstore.PGSession{}
	// It would be nicer to have `modified_on` be `NULL` but we use `time.Time`s zero value instead
	// because we need to stay compatible with `pgstore` and that does not like `NULL` even though the column is nullable.
	return sessions, pgxscan.Select(
		context.Background(), self.Db, &sessions,
		`SELECT id, encode(key, 'escape') AS key, encode(data, 'escape') AS data, created_on, modified_on, expires_on
		FROM http_sessions
		WHERE expires_on <= $1 AND modified_on = $2`,
		expiry, time.Time{},
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
