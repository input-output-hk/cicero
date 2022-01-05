package service

import (
	"encoding/json"

	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type NomadEventService interface {
	Save(pgx.Tx, *nomad.Event) error
	GetLastNomadEvent() (uint64, error)
	GetEventAllocByNomadJobId(uuid.UUID) (map[string]domain.AllocWrapper, error)
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

func (n *nomadEventService) Save(tx pgx.Tx, event *nomad.Event) error {
	n.logger.Debug().Msgf("Saving new NomadEvent %d", event.Index)
	if err := n.nomadEventRepository.Save(tx, event); err != nil {
		return errors.WithMessagef(err, "Could not insert NomadEvent")
	}
	n.logger.Debug().Msgf("Created NomadEvent %d", event.Index)
	return nil
}

func (n *nomadEventService) GetLastNomadEvent() (uint64, error) {
	n.logger.Debug().Msg("Get last Nomad Event")
	return n.nomadEventRepository.GetLastNomadEvent()
}

func (n *nomadEventService) GetEventAllocByNomadJobId(nomadJobId uuid.UUID) (map[string]domain.AllocWrapper, error) {
	allocs := map[string]domain.AllocWrapper{}
	n.logger.Debug().Msgf("Getting EventAlloc by Nomad Job ID: %d", nomadJobId)
	results, err := n.nomadEventRepository.GetEventAllocByNomadJobId(nomadJobId)
	if err != nil {
		return nil, err
	}

	for _, result := range results {
		if result["alloc"] == nil {
			continue
		}

		alloc := &nomad.Allocation{}
		err = json.Unmarshal([]byte(result["alloc"].(string)), alloc)
		if err != nil {
			return nil, err
		}

		logs, err := n.runService.RunLogs(alloc.ID, alloc.TaskGroup)
		if err != nil {
			return nil, err
		}

		allocs[alloc.Name] = domain.AllocWrapper{Alloc: alloc, Logs: logs}
	}
	n.logger.Debug().Msgf("Got EventAlloc by Nomad Job ID: %d", nomadJobId)
	return allocs, nil
}
