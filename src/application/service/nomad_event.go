package service

import (
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
	GetEventAllocationByJobId(uuid.UUID) ([]nomad.Allocation, error)
	GetLatestEventAllocationByJobId(uuid.UUID) ([]nomad.Allocation, error)
}

type nomadEventService struct {
	logger               zerolog.Logger
	nomadEventRepository repository.NomadEventRepository
}

func NewNomadEventService(db config.PgxIface, logger *zerolog.Logger) NomadEventService {
	return &nomadEventService{
		logger:               logger.With().Str("component", "NomadEventService").Logger(),
		nomadEventRepository: persistence.NewNomadEventRepository(db),
	}
}

func (n nomadEventService) WithQuerier(querier config.PgxIface) NomadEventService {
	return &nomadEventService{
		logger:               n.logger,
		nomadEventRepository: n.nomadEventRepository.WithQuerier(querier),
	}
}

func (n nomadEventService) Save(event *domain.NomadEvent) (err error) {
	n.logger.Trace().Bytes("uid", event.Uid[:]).Uint64("index", event.Index).Msg("Saving nomad event")
	if err = n.nomadEventRepository.Save(event); err != nil {
		err = errors.WithMessagef(err, "Could not save nomad event")
		return
	}
	n.logger.Trace().Bytes("uid", event.Uid[:]).Uint64("index", event.Index).Msg("Saved nomad event")
	return
}

func (n nomadEventService) Update(event *domain.NomadEvent) (err error) {
	n.logger.Trace().Bytes("uid", event.Uid[:]).Bool("handled", event.Handled).Msg("Updating nomad event")
	if err = n.nomadEventRepository.Update(event); err != nil {
		err = errors.WithMessagef(err, "Could not update nomad event")
		return
	}
	n.logger.Trace().Bytes("uid", event.Uid[:]).Bool("handled", event.Handled).Msg("Updated nomad event")
	return
}

func (n nomadEventService) GetByHandled(handled bool) (events []*domain.NomadEvent, err error) {
	n.logger.Trace().Bool("handled", handled).Msg("Get nomad events by handled flag")
	if events, err = n.nomadEventRepository.GetByHandled(handled); err != nil {
		err = errors.WithMessagef(err, "Could not get nomad events by handled flag %t", handled)
		return
	}
	n.logger.Trace().Bool("handled", handled).Msg("Got nomad events by handled flag")
	return
}

func (n nomadEventService) GetLastNomadEventIndex() (uint64, error) {
	n.logger.Trace().Msg("Get last nomad event index")
	return n.nomadEventRepository.GetLastNomadEventIndex()
}

func (n nomadEventService) GetEventAllocationByJobId(jobId uuid.UUID) (results []nomad.Allocation, err error) {
	n.logger.Trace().Stringer("job-id", jobId).Msg("Get AllocationUpdated event's Allocation by job ID")
	results, err = n.nomadEventRepository.GetEventAllocationByJobId(jobId)
	if err != nil {
		return
	}
	n.logger.Trace().Stringer("job-id", jobId).Msg("Got AllocationUpdated event's Allocation by job ID")
	return
}

func (n nomadEventService) GetLatestEventAllocationByJobId(jobId uuid.UUID) (results []nomad.Allocation, err error) {
	n.logger.Trace().Stringer("job-id", jobId).Msg("Get latest AllocationUpdated event's Allocation by job ID")
	results, err = n.nomadEventRepository.GetLatestEventAllocationByJobId(jobId)
	if err != nil {
		return
	}
	n.logger.Trace().Stringer("job-id", jobId).Msg("Got latest AllocationUpdated event's Allocation by job ID")
	return
}
