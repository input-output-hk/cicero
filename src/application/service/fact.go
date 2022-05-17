package service

import (
	"context"
	"io"
	"sync"

	"cuelang.org/go/cue"
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
	GetLatestByCue(cue.Value) (domain.Fact, error)
	GetByCue(cue.Value) ([]*domain.Fact, error)
	Save(*domain.Fact, io.Reader) error
	GetInvocationInputFacts(map[string]uuid.UUID) (map[string]domain.Fact, error)
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
	self.logger.Trace().Str("id", id.String()).Msg("Getting Fact by ID")
	fact, err = self.factRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Fact with ID %q", id)
	return
}

func (self *factService) GetByRunId(id uuid.UUID) (facts []*domain.Fact, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Facts by Run ID")
	facts, err = self.factRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select Facts for Run with ID %q", id)
	return
}

func (self *factService) GetBinaryById(tx pgx.Tx, id uuid.UUID) (binary io.ReadSeekCloser, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting binary by ID")
	binary, err = self.factRepository.GetBinaryById(tx, id)
	err = errors.WithMessagef(err, "Could not select binary from Fact with ID %q", id)
	return
}

func (self *factService) Save(fact *domain.Fact, binary io.Reader) error {
	return self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		self.logger.Trace().Msg("Saving new Fact")
		if err := self.factRepository.WithQuerier(tx).Save(fact, binary); err != nil {
			return errors.WithMessagef(err, "Could not insert Fact")
		}
		self.logger.Trace().Str("id", fact.ID.String()).Msg("Created Fact")

		if _, err := self.actionService.WithQuerier(tx).InvokeCurrentActive(); err != nil {
			return errors.WithMessagef(err, "Could not invoke currently active Actions")
		}
		return nil
	})
}

func (self *factService) GetLatestByCue(value cue.Value) (fact domain.Fact, err error) {
	self.logger.Trace().Interface("cue", value).Msg("Getting latest Facts by CUE")
	fact, err = self.factRepository.GetLatestByCue(value)
	err = errors.WithMessagef(err, "Could not select latest Facts by CUE %q", value)
	return
}

func (self *factService) GetByCue(value cue.Value) (facts []*domain.Fact, err error) {
	self.logger.Trace().Interface("cue", value).Msg("Getting Facts by CUE")
	facts, err = self.factRepository.GetByCue(value)
	err = errors.WithMessagef(err, "Could not select Facts by CUE %q", value)
	return
}

// XXX Could probably be done entirely in the DB with a single SQL query
// XXX Should this be `InvocationService.GetInputsById()`?
func (self *factService) GetInvocationInputFacts(inputFactIds map[string]uuid.UUID) (map[string]domain.Fact, error) {
	inputs := map[string]domain.Fact{}

	type Res struct {
		input string
		fact  domain.Fact
		err   error
	}
	res := make(chan *Res, len(inputFactIds))

	wg := &sync.WaitGroup{}
	wg.Add(len(inputFactIds))
	for input, id := range inputFactIds {
		go func(input string, id uuid.UUID) {
			defer wg.Done()
			fact, err := self.GetById(id)
			res <- &Res{input, fact, err}
		}(input, id)
	}
	wg.Wait()

Res:
	for {
		select {
		case result := <-res:
			if result.err != nil {
				return nil, result.err
			} else {
				inputs[result.input] = result.fact
			}
		default:
			break Res
		}
	}

	return inputs, nil
}
