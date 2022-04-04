package service

import (
	"encoding/json"
	"sync"

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

	Save(*domain.NomadEvent) error
	Update(*domain.NomadEvent) error
	GetByHandled(bool) ([]*domain.NomadEvent, error)
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

func (n *nomadEventService) Save(event *domain.NomadEvent) (err error) {
	n.logger.Trace().Bytes("uid", event.Uid[:]).Uint64("index", event.Index).Msg("Saving nomad event")
	if err = n.nomadEventRepository.Save(event); err != nil {
		err = errors.WithMessagef(err, "Could not save nomad event")
		return
	}
	n.logger.Trace().Bytes("uid", event.Uid[:]).Uint64("index", event.Index).Msg("Saved nomad event")
	return
}

func (n *nomadEventService) Update(event *domain.NomadEvent) (err error) {
	n.logger.Trace().Bytes("uid", event.Uid[:]).Bool("handled", event.Handled).Msg("Updating nomad event")
	if err = n.nomadEventRepository.Update(event); err != nil {
		err = errors.WithMessagef(err, "Could not update nomad event")
		return
	}
	n.logger.Trace().Bytes("uid", event.Uid[:]).Bool("handled", event.Handled).Msg("Updated nomad event")
	return
}

func (n *nomadEventService) GetByHandled(handled bool) (events []*domain.NomadEvent, err error) {
	n.logger.Trace().Bool("handled", handled).Msg("Get nomad events by handled flag")
	if events, err = n.nomadEventRepository.GetByHandled(handled); err != nil {
		err = errors.WithMessagef(err, "Could not get nomad events by handled flag %t", handled)
		return
	}
	n.logger.Trace().Bool("handled", handled).Msg("Got nomad events by handled flag")
	return
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

	wg := &sync.WaitGroup{}
	res := make(chan domain.AllocationWithLogs, len(results))

	for _, result := range results {
		wg.Add(1)
		go func(result map[string]interface{}) {
			defer wg.Done()
			alloc := &nomad.Allocation{}
			err = json.Unmarshal([]byte(result["alloc"].(string)), alloc)
			if err != nil {
				res <- domain.AllocationWithLogs{Err: err}
				return
			}

			run, err := n.runService.GetByNomadJobId(nomadJobId)
			if err != nil {
				res <- domain.AllocationWithLogs{Err: err}
				return
			}

			logs := map[string]domain.LokiLog{}

			for taskName := range alloc.TaskResources {
				taskLogs, err := n.runService.RunLogs(alloc.ID, alloc.TaskGroup, taskName, run.CreatedAt, run.FinishedAt)
				if err != nil {
					res <- domain.AllocationWithLogs{Err: err}
					return
				}
				logs[taskName] = taskLogs
			}

			res <- domain.AllocationWithLogs{Allocation: alloc, Logs: logs}
		}(result)
	}

	wg.Wait()

	for i := 0; i < len(results); i++ {
		select {
		case result := <-res:
			if result.Err != nil {
				return nil, err
			}
			if a, found := allocs[result.ID]; found {
				if a.ModifyIndex < result.ModifyIndex {
					allocs[result.ID] = result
				}
			} else {
				allocs[result.ID] = result
			}
		}
	}

	n.logger.Trace().Str("nomad-job-id", nomadJobId.String()).Msg("Got EventAlloc by nomad job id")
	return allocs, nil
}
