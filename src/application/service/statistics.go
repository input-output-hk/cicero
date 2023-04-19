package service

import (
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"github.com/input-output-hk/cicero/src/util"
)

type StatisticsService interface {
	WithQuerier(config.PgxIface) StatisticsService

	ActionStatus([]uuid.UUID, []string, util.MayBool) (repository.ActionsStatusStatistics, error)
}

type statisticsService struct {
	logger               zerolog.Logger
	db                   config.PgxIface
	statisticsRepository repository.StatisticsRepository
}

func NewStatisticsService(db config.PgxIface, logger *zerolog.Logger) StatisticsService {
	return &statisticsService{
		logger:               logger.With().Str("component", "StatisticsService").Logger(),
		statisticsRepository: persistence.NewStatisticsRepository(db),
		db:                   db,
	}
}

func (self statisticsService) WithQuerier(querier config.PgxIface) StatisticsService {
	return statisticsService{
		logger:               self.logger,
		statisticsRepository: self.statisticsRepository.WithQuerier(querier),
		db:                   querier,
	}
}

func (self statisticsService) ActionStatus(actionIds []uuid.UUID, actionNames []string, private util.MayBool) (repository.ActionsStatusStatistics, error) {
	logger := self.logger.With().Strs("names", actionNames).Interface("ids", actionIds).Logger()
	logger.Trace().Msg("Counting statuses")
	stats, err := self.statisticsRepository.ActionStatus(actionIds, actionNames, private)
	if err != nil {
		return stats, errors.WithMessage(err, "While counting statuses")
	}
	logger.Trace().Msg("Counted statuses")
	return stats, nil
}
