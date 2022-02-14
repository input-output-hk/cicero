package service

import (
	"context"
	"io"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type FactService interface {
	WithQuerier(config.PgxIface) FactService

	GetById(uuid.UUID) (domain.Fact, error)
	GetByRunId(uuid.UUID) ([]*domain.Fact, error)
	GetBinaryById(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	GetLatestByFields([][]string) (domain.Fact, error)
	GetByFields([][]string) ([]*domain.Fact, error)
	Save(*domain.Fact, io.Reader) error
}

type factService struct {
	logger         zerolog.Logger
	factRepository repository.FactRepository
	actionService  ActionService
	db             config.PgxIface
}

func NewFactService(db config.PgxIface, actionService ActionService, logger *zerolog.Logger) FactService {
	return &factService{
		logger:         logger.With().Str("component", "FactService").Logger(),
		actionService:  actionService,
		factRepository: persistence.NewFactRepository(db),
		db:             db,
	}
}

func (self *factService) WithQuerier(querier config.PgxIface) FactService {
	return &factService{
		logger:         self.logger,
		factRepository: self.factRepository.WithQuerier(querier),
		actionService:  self.actionService.WithQuerier(querier),
		db:             querier,
	}
}

func (self *factService) GetById(id uuid.UUID) (fact domain.Fact, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting Fact by ID")
	fact, err = self.factRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Fact with ID %q", id)
	return
}

func (self *factService) GetByRunId(id uuid.UUID) (facts []*domain.Fact, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting Facts by Run ID")
	facts, err = self.factRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select Facts for Run with ID %q", id)
	return
}

func (self *factService) GetBinaryById(tx pgx.Tx, id uuid.UUID) (binary io.ReadSeekCloser, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting binary by ID")
	binary, err = self.factRepository.GetBinaryById(tx, id)
	err = errors.WithMessagef(err, "Could not select binary from Fact with ID %q", id)
	return
}

func (self *factService) Save(fact *domain.Fact, binary io.Reader) error {
	return self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		self.logger.Debug().Msg("Saving new Fact")
		if err := self.factRepository.WithQuerier(tx).Save(fact, binary); err != nil {
			return errors.WithMessagef(err, "Could not insert Fact")
		}
		self.logger.Debug().Str("id", fact.ID.String()).Msg("Created Fact")

		return self.actionService.WithQuerier(tx).InvokeCurrentActive()
	})
}

func (self *factService) GetLatestByFields(fields [][]string) (fact domain.Fact, err error) {
	self.logger.Debug().Interface("fields", fields).Msg("Getting latest Facts by fields")
	fact, err = self.factRepository.GetLatestByFields(fields)
	err = errors.WithMessagef(err, "Could not select latest Facts by fields %q", fields)
	return
}

func (self *factService) GetByFields(fields [][]string) (facts []*domain.Fact, err error) {
	self.logger.Debug().Interface("fields", fields).Msg("Getting Facts by fields")
	facts, err = self.factRepository.GetByFields(fields)
	err = errors.WithMessagef(err, "Could not select Facts by fields %q", fields)
	return
}
