package service

import (
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/gorilla/securecookie"
	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type SessionService interface {
	WithQuerier(config.PgxIface) SessionService

	GetByKey(string) (*pgstore.PGSession, error)
	GetExpiredByAndZeroModified(time.Time) ([]pgstore.PGSession, error)
	Update(*pgstore.PGSession, string, []securecookie.Codec, func(map[any]any) error) error
}

type sessionService struct {
	logger            zerolog.Logger
	sessionRepository repository.SessionRepository
}

func NewSessionService(db *pgxpool.Pool, logger *zerolog.Logger) SessionService {
	return &sessionService{
		logger:            logger.With().Str("component", "SessionService").Logger(),
		sessionRepository: persistence.NewSessionRepository(db),
	}
}

func (self *sessionService) WithQuerier(querier config.PgxIface) SessionService {
	return &sessionService{
		self.logger,
		self.sessionRepository.WithQuerier(querier),
	}
}

func (self sessionService) GetByKey(key string) (session *pgstore.PGSession, err error) {
	logger := self.logger.With().Str("key", key).Logger()
	logger.Trace().Msg("Getting session by key")
	session, err = self.sessionRepository.GetByKey(key)
	if err != nil {
		err = errors.WithMessagef(err, "While getting session by key %q", key)
		return
	}
	logger.Trace().Msg("Got session by key")
	return
}

func (self sessionService) GetExpiredByAndZeroModified(expiry time.Time) (sessions []pgstore.PGSession, err error) {
	logger := self.logger.With().Stringer("expiry", expiry).Logger()
	logger.Trace().Msg("Getting session that will have expired and that have never been modified")
	sessions, err = self.sessionRepository.GetExpiredByAndZeroModified(expiry)
	if err != nil {
		err = errors.WithMessagef(err, "While getting sessions that will have expired by %s and that have never been modified", expiry)
		return
	}
	logger.Trace().Int("count", len(sessions)).Msg("Got session that will have expired and that have never been modified")
	return
}

func (self sessionService) Update(session *pgstore.PGSession, name string, codecs []securecookie.Codec, modifyData func(map[any]any) error) error {
	logger := self.logger.With().Str("key", session.Key).Logger()
	logger.Trace().Msg("Updating session")

	data := make(map[any]any)
	if err := securecookie.DecodeMulti(name, string(session.Data), &data, codecs...); err != nil {
		return errors.WithMessage(err, "While decoding session")
	}
	if err := modifyData(data); err != nil {
		return errors.WithMessage(err, "While modifying data")
	}
	if encoded, err := securecookie.EncodeMulti(name, data, codecs...); err != nil {
		return errors.WithMessage(err, "While encoding session")
	} else {
		session.Data = encoded
	}

	if err := self.sessionRepository.Update(*session); err != nil {
		return errors.WithMessagef(err, "While updating session %s", session.Key)
	}

	logger.Trace().Msg("Updated session")
	return nil
}
