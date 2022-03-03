package service

import (
	"encoding/json"

	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type NomadEventService interface {
	WithQuerier(config.PgxIface) NomadEventService

	Save(*nomad.Event) error
	GetLastNomadEventIndex() (uint64, error)
	GetEventAllocByNomadJobId(id uuid.UUID) (map[string]domain.AllocationWithLogs, error)
}

type nomadEventService struct {
	logger               zerolog.Logger
	nomadEventRepository repository.NomadEventRepository
	runService           RunService
}

func NewNomadEventService(db config.PgxIface, runService RunService, logger *zerolog.Logger) NomadEventService {
	return &nomadEventService{
		logger:               logger.With().Str("component", "NomadEventService").Logger(),
		nomadEventRepository: persistence.NewNomadEventRepository(db),
		runService:           runService,
	}
}

func (n *nomadEventService) WithQuerier(querier config.PgxIface) NomadEventService {
	return &nomadEventService{
		logger:               n.logger,
		nomadEventRepository: n.nomadEventRepository.WithQuerier(querier),
		runService:           n.runService.WithQuerier(querier),
	}
}

func (n *nomadEventService) Save(event *nomad.Event) error {
	n.logger.Trace().Uint64("index", event.Index).Msg("Saving new nomad event")
	if err := n.nomadEventRepository.Save(event); err != nil {
		return errors.WithMessagef(err, "Could not insert NomadEvent")
	}
	n.logger.Trace().Uint64("index", event.Index).Msg("Saved new nomad event")
	return nil
}

func (n *nomadEventService) GetLastNomadEventIndex() (uint64, error) {
	n.logger.Trace().Msg("Get last nomad event index")
	return n.nomadEventRepository.GetLastNomadEventIndex()
}

func (n *nomadEventService) GetEventAllocByNomadJobId(nomadJobId uuid.UUID) (map[string]domain.AllocationWithLogs, error) {
	n.logger.Trace().Str("nomad-job-id", nomadJobId.String()).Msg("Getting EventAlloc by nomad job id")

	allocs := map[string]domain.AllocationWithLogs{}
	results, err := n.nomadEventRepository.GetEventAllocByNomadJobId(nomadJobId)
	if err != nil {
		return nil, err
	}

	for _, result := range results {
		alloc := &nomad.Allocation{}
		err = json.Unmarshal([]byte(result["alloc"].(string)), alloc)
		if err != nil {
			return nil, err
		}

		run, err := n.runService.GetByNomadJobId(nomadJobId)
		if err != nil {
			return nil, err
		}

		logs := map[string]*domain.LokiLog{}

		for taskName := range alloc.TaskResources {
			taskLogs, err := n.runService.RunLogs(alloc.ID, alloc.TaskGroup, taskName, run.CreatedAt, run.FinishedAt)
			if err != nil {
				return nil, err
			}
			logs[taskName] = taskLogs
		}

		allocs[alloc.ID] = domain.AllocationWithLogs{Allocation: alloc, Logs: logs}
	}

	n.logger.Trace().Str("nomad-job-id", nomadJobId.String()).Msg("Got EventAlloc by nomad job id")
	return allocs, nil
}
