package service

import (
	"context"
	"fmt"
	"io"

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
	withQuerier(config.PgxIface, FactServiceCyclicDependencies) FactService

	GetById(uuid.UUID) (*domain.Fact, error)
	GetByIds(map[string]uuid.UUID) (map[string]domain.Fact, error)
	GetByRunId(uuid.UUID) ([]domain.Fact, error)
	GetBinaryById(pgx.Tx, uuid.UUID) (io.ReadSeekCloser, error)
	GetLatestByCue(cue.Value) (*domain.Fact, error)
	GetByCue(cue.Value) ([]domain.Fact, error)
	Save(*domain.Fact, io.Reader) ([]domain.Invocation, InvokeRunFunc, error)
	Match(*domain.Fact, cue.Value) (cue.Value, error, error)
}

type FactServiceCyclicDependencies struct {
	actionService *ActionService
}

type factService struct {
	logger         zerolog.Logger
	factRepository repository.FactRepository
	db             config.PgxIface
	FactServiceCyclicDependencies
}

func NewFactService(db config.PgxIface, actionService *ActionService, logger *zerolog.Logger) FactService {
	return &factService{
		logger:         logger.With().Str("component", "FactService").Logger(),
		factRepository: persistence.NewFactRepository(db),
		db:             db,
		FactServiceCyclicDependencies: FactServiceCyclicDependencies{
			actionService: actionService,
		},
	}
}

func (self factService) WithQuerier(querier config.PgxIface) FactService {
	return self.withQuerier(querier, FactServiceCyclicDependencies{})
}

func (self factService) withQuerier(querier config.PgxIface, cyclicDeps FactServiceCyclicDependencies) FactService {
	result := factService{
		logger:                        self.logger,
		factRepository:                self.factRepository.WithQuerier(querier),
		db:                            querier,
		FactServiceCyclicDependencies: cyclicDeps,
	}

	if result.actionService == nil {
		result.actionService = new(ActionService)
		*result.actionService = (*self.actionService).withQuerier(querier, ActionServiceCyclicDependencies{})
	}

	return &result
}

func (self factService) GetById(id uuid.UUID) (fact *domain.Fact, err error) {
	self.logger.Trace().Stringer("id", id).Msg("Getting Fact by ID")
	fact, err = self.factRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Fact with ID %q", id)
	return
}

func (self factService) GetByIds(ids map[string]uuid.UUID) (facts map[string]domain.Fact, err error) {
	self.logger.Trace().Interface("ids", ids).Msg("Getting Facts by IDs")
	facts, err = self.factRepository.GetByIds(ids)
	err = errors.WithMessagef(err, "Could not select Facts by IDs %q", ids)
	return
}

func (self factService) GetByRunId(id uuid.UUID) (facts []domain.Fact, err error) {
	self.logger.Trace().Stringer("id", id).Msg("Getting Facts by Run ID")
	facts, err = self.factRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select Facts for Run with ID %q", id)
	return
}

func (self factService) GetBinaryById(tx pgx.Tx, id uuid.UUID) (binary io.ReadSeekCloser, err error) {
	self.logger.Trace().Stringer("id", id).Msg("Getting binary by ID")
	binary, err = self.factRepository.GetBinaryById(tx, id)
	err = errors.WithMessagef(err, "Could not select binary from Fact with ID %q", id)
	return
}

func (self factService) Save(fact *domain.Fact, binary io.Reader) ([]domain.Invocation, InvokeRunFunc, error) {
	var runFunc InvokeRunFunc
	var invocations []domain.Invocation

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*factService)

		self.logger.Trace().Msg("Saving new Fact")
		if err := txSelf.factRepository.Save(fact, binary); err != nil {
			return errors.WithMessage(err, "Could not insert Fact")
		}
		self.logger.Trace().Stringer("id", fact.ID).Msg("Created Fact")

		if invocations_, runFunc_, err := (*txSelf.actionService).InvokeCurrentActive(); err != nil {
			return errors.WithMessage(err, "Could not invoke currently active Actions")
		} else {
			invocations = invocations_
			runFunc = runFunc_
		}
		return nil
	}); err != nil {
		return invocations, runFunc, err
	}

	return invocations, runFunc, nil
}

func (self factService) GetLatestByCue(value cue.Value) (fact *domain.Fact, err error) {
	self.logger.Trace().Str("cue", fmt.Sprint(value)).Msg("Getting latest Fact by CUE")
	fact, err = self.factRepository.GetLatestByCue(value)
	err = errors.WithMessagef(err, "Could not select latest Fact by CUE %q", value)
	return
}

func (self factService) GetByCue(value cue.Value) (facts []domain.Fact, err error) {
	self.logger.Trace().Str("cue", fmt.Sprint(value)).Msg("Getting Facts by CUE")
	facts, err = self.factRepository.GetByCue(value)
	err = errors.WithMessagef(err, "Could not select Facts by CUE %q", value)
	return
}

func (self factService) Match(fact *domain.Fact, match cue.Value) (cue.Value, error, error) {
	factCue := match.Context().Encode(fact.Value)
	if err := factCue.Err(); err != nil {
		return cue.Value{}, nil, err
	}

	unified := match.Unify(factCue)

	return unified, unified.Validate(cue.Concrete(true)), nil
}
