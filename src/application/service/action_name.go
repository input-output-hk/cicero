package service

import (
	"context"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type ActionNameService interface {
	WithQuerier(config.PgxIface) ActionNameService

	GetByActionId(uuid.UUID) (*domain.ActionName, error)
	Get(string) (domain.ActionName, error)
	Update(string, func(*domain.ActionName, config.PgxIface) error) error
}

type actionNameService struct {
	logger               zerolog.Logger
	actionNameRepository repository.ActionNameRepository
	db                   config.PgxIface
}

func NewActionNameService(db config.PgxIface, logger *zerolog.Logger) ActionNameService {
	return &actionNameService{
		logger:               logger.With().Str("component", "ActionNameService").Logger(),
		actionNameRepository: persistence.NewActionNameRepository(db),
		db:                   db,
	}
}

func (self actionNameService) WithQuerier(querier config.PgxIface) ActionNameService {
	return &actionNameService{
		logger:               self.logger,
		actionNameRepository: self.actionNameRepository.WithQuerier(querier),
		db:                   querier,
	}
}

func (self actionNameService) GetByActionId(id uuid.UUID) (*domain.ActionName, error) {
	logger := self.logger.With().Stringer("action-id", id).Logger()
	logger.Trace().Msg("Getting Action Name by Action ID")
	actionName, err := self.actionNameRepository.GetByActionId(id)
	if err != nil {
		return actionName, err
	}
	logger.Trace().Msg("Got Action Name by Action ID")
	return actionName, nil
}

func (self actionNameService) Get(name string) (domain.ActionName, error) {
	logger := self.logger.With().Str("name", name).Logger()
	logger.Trace().Msg("Getting Action Name")
	actionName, err := self.actionNameRepository.Get(name)
	if actionName == nil {
		actionName = &domain.ActionName{Name: name}
	}
	if err != nil {
		return *actionName, err
	}
	logger.Trace().Msg("Got Action Name")
	return *actionName, nil
}

func (self actionNameService) Update(name string, update func(*domain.ActionName, config.PgxIface) error) error {
	logger := self.logger.With().Str("name", name).Logger()
	logger.Trace().Msg("Updating Action Name")
	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*actionNameService)

		actionName, err := txSelf.actionNameRepository.Get(name)
		if err != nil {
			return err
		}
		if actionName == nil {
			actionName = &domain.ActionName{Name: name}
		}

		if err := update(actionName, tx); err != nil {
			return errors.WithMessage(err, "While updating Action Name")
		}

		return txSelf.actionNameRepository.Save(*actionName)
	}); err != nil {
		return err
	}
	logger.Trace().Msg("Updated Action Name")
	return nil
}
