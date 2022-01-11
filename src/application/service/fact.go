package service

import (
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"io"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type FactService interface {
	GetById(uuid.UUID) (domain.Fact, error)
	GetBinaryByIdAndTx(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	GetLatestByFields(pgx.Tx, [][]string) (domain.Fact, error)
	GetByFields(pgx.Tx, [][]string) ([]*domain.Fact, error)
	Save(pgx.Tx, *domain.Fact, io.Reader) error
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

func (self *factService) GetById(id uuid.UUID) (fact domain.Fact, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting Fact by ID")
	fact, err = self.factRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select existing Fact for ID: %s", id)
	}
	return
}

// GetBinaryByIdAndTx require a transaction (Tx) because is the is necessary to get a LargeObjects
func (self *factService) GetBinaryByIdAndTx(tx pgx.Tx, id uuid.UUID) (binary io.ReadSeekCloser, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting binary by ID")
	binary, err = self.factRepository.GetBinaryByIdAndTx(tx, id)
	if err != nil {
		self.logger.Err(err).Str("id", id.String()).Msgf("Could not select Binary from Fact")
		err = errors.WithMessagef(err, "Could not select Binary from Fact for ID: %s", id)
	}
	return
}

func (self *factService) Save(tx pgx.Tx, fact *domain.Fact, binary io.Reader) error {
	self.logger.Debug().Msg("Saving new Fact")
	if err := self.factRepository.Save(tx, fact, binary); err != nil {
		return errors.WithMessagef(err, "Could not insert Fact")
	}
	self.logger.Debug().Str("id", fact.ID.String()).Msg("Created Fact")

	if actions, err := self.actionService.GetCurrent(); err != nil {
		return err
	} else {
		for _, action := range actions {
			if err := self.actionService.Invoke(tx, action); err != nil {
				return err
			}
		}
	}

	return nil
}

func (self *factService) GetLatestByFields(tx pgx.Tx, fields [][]string) (fact domain.Fact, err error) {
	self.logger.Debug().Interface("fields", fields).Msg("Getting latest Facts by fields")
	fact, err = self.factRepository.GetLatestByFields(tx, fields)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select latest Facts by fields %q", fields)
	}
	return
}

func (self *factService) GetByFields(tx pgx.Tx, fields [][]string) (facts []*domain.Fact, err error) {
	self.logger.Debug().Interface("fields", fields).Msg("Getting Facts by fields")
	facts, err = self.factRepository.GetByFields(tx, fields)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select Facts by fields %q", fields)
	}
	return
}
