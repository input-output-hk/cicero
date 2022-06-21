package service

import (
	"fmt"
	"strconv"

	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type InvocationService interface {
	WithQuerier(config.PgxIface) InvocationService
	withQuerier(config.PgxIface, InvocationServiceCyclicDependencies) InvocationService

	GetById(uuid.UUID) (*domain.Invocation, error)
	GetByActionId(uuid.UUID, *repository.Page) ([]*domain.Invocation, error)
	GetLatestByActionId(uuid.UUID) (*domain.Invocation, error)
	GetAll(*repository.Page) ([]*domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, *bool, *repository.Page) ([]*domain.Invocation, error)
	GetInputFactIdsById(uuid.UUID) (map[string]uuid.UUID, error)
	GetOutputById(uuid.UUID) (domain.OutputDefinition, error)
	Save(*domain.Invocation, map[string]*domain.Fact) error
	End(uuid.UUID) error
	GetLog(domain.Invocation) (LokiLog, error)
}

type InvocationServiceCyclicDependencies struct {
	actionService *ActionService
	factService   *FactService
}

type invocationService struct {
	logger               zerolog.Logger
	invocationRepository repository.InvocationRepository
	db                   config.PgxIface
	lokiService          LokiService
	InvocationServiceCyclicDependencies
}

func NewInvocationService(db config.PgxIface, lokiService LokiService, actionService *ActionService, factService *FactService, logger *zerolog.Logger) InvocationService {
	return &invocationService{
		logger:               logger.With().Str("component", "InvocationService").Logger(),
		invocationRepository: persistence.NewInvocationRepository(db),
		db:                   db,
		lokiService:          lokiService,
		InvocationServiceCyclicDependencies: InvocationServiceCyclicDependencies{
			actionService: actionService,
			factService:   factService,
		},
	}
}

func (self invocationService) WithQuerier(querier config.PgxIface) InvocationService {
	return self.withQuerier(querier, InvocationServiceCyclicDependencies{})
}

func (self invocationService) withQuerier(querier config.PgxIface, cyclicDeps InvocationServiceCyclicDependencies) InvocationService {
	result := invocationService{
		logger:                              self.logger,
		invocationRepository:                self.invocationRepository.WithQuerier(querier),
		db:                                  querier,
		InvocationServiceCyclicDependencies: cyclicDeps,
	}

	if result.actionService == nil {
		r := InvocationService(result)
		result.actionService = new(ActionService)
		*result.actionService = (*self.actionService).withQuerier(querier, ActionServiceCyclicDependencies{
			invocationService: &r,
		})
	}
	if result.factService == nil {
		result.factService = new(FactService)
		*result.factService = (*self.factService).withQuerier(querier, FactServiceCyclicDependencies{
			actionService: result.actionService,
		})
	}

	return &result
}

func (self invocationService) GetById(id uuid.UUID) (invocation *domain.Invocation, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Invocation by ID")
	invocation, err = self.invocationRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Invocation for ID %q", id)
	return
}

func (self invocationService) GetByActionId(id uuid.UUID, page *repository.Page) (invocations []*domain.Invocation, err error) {
	self.logger.Trace().Str("id", id.String()).Int("offset", page.Offset).Int("limit", page.Limit).Msgf("Getting Invocation by Action ID")
	invocations, err = self.invocationRepository.GetByActionId(id, page)
	err = errors.WithMessagef(err, "Could not select existing Invocation by Action ID %q with offset %d and limit %d", id, page.Offset, page.Limit)
	return
}

func (self invocationService) GetLatestByActionId(id uuid.UUID) (invocation *domain.Invocation, err error) {
	self.logger.Trace().Str("action-id", id.String()).Msg("Getting latest Invocation by Action ID")
	invocation, err = self.invocationRepository.GetLatestByActionId(id)
	err = errors.WithMessagef(err, "Could not select latest Invocation by Action ID %q", id)
	return
}

func (self invocationService) GetAll(page *repository.Page) (invocations []*domain.Invocation, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting all Invocations")
	invocations, err = self.invocationRepository.GetAll(page)
	err = errors.WithMessagef(err, "Could not select existing Invocations with offset %d and limit %d", page.Offset, page.Limit)
	return
}

func (self invocationService) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, ok *bool, page *repository.Page) (invocations []*domain.Invocation, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Interface("input-fact-ids", factIds).Bool("recursive", recursive).Interface("ok", ok).Msg("Getting Invocations by input Fact IDs")
	invocations, err = self.invocationRepository.GetByInputFactIds(factIds, recursive, ok, page)
	errMsg := "Could not select Invocations by input fact IDs %q (recursively: %t, ok: "
	if ok != nil {
		errMsg = string(strconv.AppendBool([]byte(errMsg), *ok))
	} else {
		errMsg += "nil"
	}
	errMsg += ") with offset %d and limit %d"
	err = errors.WithMessagef(err, errMsg, factIds, recursive, page.Offset, page.Limit)
	return
}

func (self invocationService) GetInputFactIdsById(id uuid.UUID) (inputFactIds map[string]uuid.UUID, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Invocation input fact IDs by Nomad Job ID")
	inputFactIds, err = self.invocationRepository.GetInputFactIdsById(id)
	err = errors.WithMessagef(err, "Could not select Invocation input fact IDs by Nomad Job ID %q", id)
	return
}

func (self invocationService) GetOutputById(id uuid.UUID) (domain.OutputDefinition, error) {
	self.logger.Trace().Str("id", id.String()).Msg("Evaluating output for ID")
	if action, err := (*self.actionService).GetByInvocationId(id); err != nil {
		return domain.OutputDefinition{}, err
	} else if inputFactIds, err := self.GetInputFactIdsById(id); err != nil {
		return domain.OutputDefinition{}, err
	} else if inputs, err := (*self.factService).GetInvocationInputFacts(inputFactIds); err != nil {
		return domain.OutputDefinition{}, err
	} else {
		return action.InOut.Output(inputs), nil
	}
}

func (self invocationService) Save(invocation *domain.Invocation, inputs map[string]*domain.Fact) error {
	self.logger.Trace().Msg("Saving new Invocation")
	if err := self.invocationRepository.Save(invocation, inputs); err != nil {
		return errors.WithMessagef(err, "Could not insert Invocation")
	}
	self.logger.Trace().Str("id", invocation.Id.String()).Msg("Created Invocation")
	return nil
}

func (self invocationService) End(id uuid.UUID) error {
	self.logger.Trace().Stringer("id", id).Msg("Ending Invocation")
	if err := self.invocationRepository.End(id); err != nil {
		return errors.WithMessagef(err, "Could not end Invocation")
	}
	self.logger.Trace().Stringer("id", id).Msg("Ended Invocation")
	return nil
}

func (self invocationService) GetLog(invocation domain.Invocation) (LokiLog, error) {
	return self.lokiService.QueryRangeLog(
		fmt.Sprintf(`{cicero=~"eval(-transform)?",invocation=%q}`, invocation.Id),
		invocation.CreatedAt, nil,
	)
}
