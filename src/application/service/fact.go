package service

import (
	"context"
	"encoding/json"
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
	Save(*domain.Fact, io.Reader) ([]domain.Invocation, InvokeRunFunc, error)
	Match(*domain.Fact, cue.Value) (error, error)
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
		r := FactService(result)
		result.actionService = new(ActionService)
		*result.actionService = (*self.actionService).withQuerier(querier, ActionServiceCyclicDependencies{
			factService: &r,
		})
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

		if err := (*txSelf.actionService).UpdateSatisfaction(fact); err != nil {
			return errors.WithMessage(err, "Could not update satisfaction")
		}

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

func (self factService) Match(fact *domain.Fact, match cue.Value) (error, error) {
	factValueJson, err := json.Marshal(fact.Value)
	if err != nil {
		return nil, errors.WithMessage(err, "Could not marshal fact to JSON")
	}

	// Read these to understand better:
	// - https://github.com/cue-lang/cue/issues/375#issuecomment-873387284
	// - https://github.com/cue-lang/cue/issues/1259
	// - https://github.com/cue-lang/cue/issues/1470

	// A bit hacky but easy to understand. Not sure how else to close only `factCue`.
	factCue := match.Context().CompileString("close(" + string(factValueJson) + ")")
	if err := factCue.Err(); err != nil {
		return nil, errors.WithMessage(err, "Could not create CUE value from fact")
	}

	// Cannot use `match.Subsume(factCue)` (no matter which options like `cue.Schema()` and `cue.Final()`)
	// because subsumption does not work with string interpolation like `a: "\(b)", b: string` in `match`.
	return match.Unify(factCue).Validate(cue.Concrete(true)), nil
}
