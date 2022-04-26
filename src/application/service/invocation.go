package service

import (
	"context"
	"strconv"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type InvocationService interface {
	WithQuerier(config.PgxIface) InvocationService

	GetById(uuid.UUID) (domain.Invocation, error)
	GetByActionId(uuid.UUID, *repository.Page) ([]*domain.Invocation, error)
	GetLatestByActionId(uuid.UUID) (domain.Invocation, error)
	GetAll(*repository.Page) ([]*domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, *bool, *repository.Page) ([]*domain.Invocation, error)
	GetInputFactIdsById(uuid.UUID) (repository.InvocationInputFactIds, error)
	Save(*domain.Invocation, map[string]interface{}) error
	Update(*domain.Invocation) error
}

type invocationService struct {
	logger               zerolog.Logger
	db                   config.PgxIface
	invocationRepository repository.InvocationRepository
}

func NewInvocationService(db config.PgxIface, logger *zerolog.Logger) InvocationService {
	return &invocationService{
		logger:               logger.With().Str("component", "InvocationService").Logger(),
		invocationRepository: persistence.NewInvocationRepository(db),
		db:                   db,
	}
}

func (self *invocationService) WithQuerier(querier config.PgxIface) InvocationService {
	return &invocationService{
		logger:               self.logger,
		invocationRepository: self.invocationRepository.WithQuerier(querier),
		db:                   querier,
	}
}

func (self *invocationService) GetById(id uuid.UUID) (invocation domain.Invocation, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Invocation by ID")
	invocation, err = self.invocationRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Invocation for ID %q", id)
	return
}

func (self *invocationService) GetByActionId(id uuid.UUID, page *repository.Page) (invocations []*domain.Invocation, err error) {
	self.logger.Trace().Str("id", id.String()).Int("offset", page.Offset).Int("limit", page.Limit).Msgf("Getting Invocation by Action ID")
	invocations, err = self.invocationRepository.GetByActionId(id, page)
	err = errors.WithMessagef(err, "Could not select existing Invocation by Action ID %q with offset %d and limit %d", id, page.Offset, page.Limit)
	return
}

func (self *invocationService) GetLatestByActionId(id uuid.UUID) (invocation domain.Invocation, err error) {
	self.logger.Trace().Str("action-id", id.String()).Msg("Getting latest Invocation by Action ID")
	invocation, err = self.invocationRepository.GetLatestByActionId(id)
	err = errors.WithMessagef(err, "Could not select latest Invocation by Action ID %q", id)
	return
}

func (self *invocationService) GetAll(page *repository.Page) (invocations []*domain.Invocation, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting all Invocations")
	invocations, err = self.invocationRepository.GetAll(page)
	err = errors.WithMessagef(err, "Could not select existing Invocations with offset %d and limit %d", page.Offset, page.Limit)
	return
}

func (self *invocationService) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, ok *bool, page *repository.Page) (invocations []*domain.Invocation, err error) {
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

func (self *invocationService) GetInputFactIdsById(id uuid.UUID) (inputFactIds repository.InvocationInputFactIds, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Invocation input fact IDs by Nomad Job ID")
	inputFactIds, err = self.invocationRepository.GetInputFactIdsById(id)
	err = errors.WithMessagef(err, "Could not select Invocation input fact IDs by Nomad Job ID %q", id)
	return
}

func (self *invocationService) Save(invocation *domain.Invocation, inputs map[string]interface{}) error {
	self.logger.Trace().Msg("Saving new Invocation")
	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.invocationRepository.WithQuerier(tx).Save(invocation, inputs); err != nil {
			return errors.WithMessagef(err, "Could not insert Invocation")
		}
		return nil
	}); err != nil {
		return err
	}
	self.logger.Trace().Str("id", invocation.Id.String()).Msg("Created Invocation")
	return nil
}

func (self *invocationService) Update(invocation *domain.Invocation) error {
	self.logger.Trace().Str("id", invocation.Id.String()).Msg("Updating Invocation")
	if err := self.invocationRepository.Update(invocation); err != nil {
		return errors.WithMessagef(err, "Could not update Invocation with ID %q", invocation.Id)
	}
	self.logger.Trace().Str("id", invocation.Id.String()).Msg("Updated Invocation")
	return nil
}
