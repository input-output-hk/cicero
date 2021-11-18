package service

import (
	"encoding/json"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/repository"

	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"log"
	"os"
)

type NomadEventService interface {
	Save(pgx.Tx, *nomad.Event) error
	GetLastNomadEvent() (uint64, error)
	GetEventAllocByWorkflowId(uint64) (map[string]AllocWrapper, error)
}

type nomadEventService struct {
	logger               *log.Logger
	nomadEventRepository repository.NomadEventRepository
	actionService        ActionService
}

func NewNomadEventService(db *pgxpool.Pool, actionService ActionService) NomadEventService {
	return &nomadEventService{
		logger:               log.New(os.Stderr, "NomadEventService: ", log.LstdFlags),
		nomadEventRepository: repository.NewNomadEventRepository(db),
		actionService:        actionService,
	}
}

func (n *nomadEventService) Save(tx pgx.Tx, event *nomad.Event) error {
	n.logger.Printf("Saving new NomadEvent %#v", event)
	if err := n.nomadEventRepository.Save(tx, event); err != nil {
		return errors.WithMessagef(err, "Couldn't insert NomadEvent")
	}
	n.logger.Printf("Created NomadEvent %#v", event)
	return nil
}

func (n *nomadEventService) GetLastNomadEvent() (uint64, error) {
	n.logger.Printf("Get last Nomad Event")
	return n.nomadEventRepository.GetLastNomadEvent()
}

type AllocWrapper struct {
	Alloc *nomad.Allocation
	Logs  *LokiOutput
}

func (n *nomadEventService) GetEventAllocByWorkflowId(workflowId uint64) (map[string]AllocWrapper, error) {
	allocs := map[string]AllocWrapper{}
	results := []map[string]interface{}{}
	n.logger.Printf("Get EventAlloc by WorkflowId: %d", workflowId)
	results, err := n.nomadEventRepository.GetEventAllocByWorkflowId(workflowId)
	if err != nil {
		return nil, err
	}

	for _, result := range results {
		alloc := &nomad.Allocation{}
		err = json.Unmarshal([]byte(result["alloc"].(string)), alloc)
		if err != nil {
			return nil, err
		}

		logs, err := n.actionService.ActionLogs(alloc.ID, alloc.TaskGroup)
		if err != nil {
			return nil, err
		}

		allocs[result["name"].(string)] = AllocWrapper{Alloc: alloc, Logs: logs}
	}
	n.logger.Printf("EventAlloc by WorkflowId: %d - %#v", workflowId, allocs)
	return allocs, nil
}
