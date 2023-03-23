package service

import (
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type SessionService interface {
	WithQuerier(config.PgxIface) SessionService

	GetExpiredBy(time.Time) ([]pgstore.PGSession, error)
	Update(pgstore.PGSession) error
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

func (self sessionService) GetExpiredBy(expiry time.Time) (sessions []pgstore.PGSession, err error) {
	logger := self.logger.With().Stringer("expiry", expiry).Logger()
	logger.Trace().Msg("Getting session that will have expired")
	sessions, err = self.sessionRepository.GetExpiredBy(expiry)
	if err != nil {
		err = errors.WithMessagef(err, "While getting sessions that will have expired by %s", expiry)
		return
	}
	logger.Trace().Int("count", len(sessions)).Msg("Got session that will have expired")
	return
}

func (self sessionService) Update(session pgstore.PGSession) (err error) {
	logger := self.logger.With().Str("key", session.Key).Logger()
	logger.Trace().Msg("Updating session")
	err = self.sessionRepository.Update(session)
	if err != nil {
		err = errors.WithMessagef(err, "While updating session %s", session.Key)
		return
	}
	logger.Trace().Msg("Updated session")
	return
}
