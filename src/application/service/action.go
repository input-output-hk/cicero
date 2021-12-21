package service

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"cuelang.org/go/cue"
	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type ActionService interface {
	GetById(uuid.UUID) (domain.Action, error)
	GetLatestByName(string) (domain.Action, error)
	GetAll() ([]*domain.Action, error)
	GetCurrent() ([]*domain.Action, error)
	Save(pgx.Tx, *domain.Action) error
	IsRunnable(*domain.Action) (bool, map[string]interface{}, error)
}

type actionService struct {
	logger           *log.Logger
	actionRepository repository.ActionRepository
	factRepository   repository.FactRepository
}

func NewActionService(db config.PgxIface) ActionService {
	return &actionService{
		logger:           log.New(os.Stderr, "ActionService: ", log.LstdFlags),
		actionRepository: persistence.NewActionRepository(db),
		factRepository:   persistence.NewFactRepository(db),
	}
}

func (self *actionService) GetById(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Printf("Getting Action by ID %s", id)
	action, err = self.actionRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	}
	return
}

func (self *actionService) GetLatestByName(name string) (action domain.Action, err error) {
	self.logger.Printf("Getting latest Action by name %s", name)
	action, err = self.actionRepository.GetLatestByName(name)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select latest Action for name %q", name)
	}
	return
}

func (self *actionService) GetAll() ([]*domain.Action, error) {
	self.logger.Printf("Getting all Actions")
	return self.actionRepository.GetAll()
}

func (self *actionService) Save(tx pgx.Tx, action *domain.Action) error {
	self.logger.Printf("Saving new Action named %s", action.Name)
	if err := self.actionRepository.Save(tx, action); err != nil {
		return errors.WithMessagef(err, "Could not insert Action")
	}
	self.logger.Printf("Created Action %s", action.ID)
	return nil
}

func (self *actionService) GetCurrent() (actions []*domain.Action, err error) {
	self.logger.Println("Getting current Actions")
	actions, err = self.actionRepository.GetCurrent()
	if err != nil {
		err = errors.WithMessagef(err, "Could not select current Actions")
	}
	return
}

func (self *actionService) IsRunnable(action *domain.Action) (bool, map[string]interface{}, error) {
	self.logger.Printf("Checking whether Action %s is runnable", action.ID)

	inputs := map[string]interface{}{}

	// XXX We only check that the required paths are present.
	// In the future we could extend this to also check some values.
	// For that we would have to convert CUE constraints to SQL clauses
	// which sounds non-trivial and it would only help improve performance,
	// except we define a subset of constraints as the only conditions we support
	// and no longer run the CUE filter over the selected facts afterwards.

	// FIXME race condition: facts may change in between checking whether each input is satisfied

	for name, input := range action.Inputs {
		switch input.Select {
		case domain.InputDefinitionSelectLatest:
			if satisfied, fact, err := self.isInputSatisfiedLatest(&input.Match); err != nil {
				return false, nil, err
			} else if satisfied != input.Not {
				if !input.Not {
					inputs[name] = fact
				}
			} else {
				return false, nil, nil
			}
		case domain.InputDefinitionSelectAll:
			if satisfied, facts, err := self.isInputSatisfied(&input.Match); err != nil {
				return false, nil, err
			} else if satisfied != input.Not {
				if !input.Not {
					inputs[name] = facts
				}
			} else {
				return false, nil, nil
			}
		default:
			return false, nil, fmt.Errorf("InputDefinitionSelect with unknown value %d", input.Select)
		}
	}

	return true, inputs, nil
}

func (self *actionService) isInputSatisfiedLatest(value *cue.Value) (bool, *domain.Fact, error) {
	if fact, err := self.factRepository.GetLatestByFields(collectFieldPaths(value)); err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return false, nil, nil
		}
		return false, nil, err
	} else if match, err := matchFact(value, &fact); err != nil {
		return false, &fact, err
	} else {
		return match, &fact, nil
	}
}

func (self *actionService) isInputSatisfied(value *cue.Value) (bool, []*domain.Fact, error) {
	if facts, err := self.factRepository.GetByFields(collectFieldPaths(value)); err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return false, nil, nil
		}
		return false, nil, err
	} else {
		for _, fact := range facts {
			if match, err := matchFact(value, fact); err != nil {
				return false, facts, err
			} else if !match {
				return false, facts, nil
			}
		}
		return true, facts, nil
	}
}

func matchFact(value *cue.Value, fact *domain.Fact) (bool, error) {
	if factValue, err := json.Marshal(fact.Value); err != nil {
		return false, err
	} else if factCue := value.Context().CompileBytes(factValue); factCue.Err() != nil {
		return false, err
	} else if value.Subsume(factCue) != nil {
		return false, nil
	} else {
		return true, nil
	}
}

func collectFieldPaths(value *cue.Value) (paths [][]string) {
	if strukt, err := value.Struct(); err != nil {
		return
	} else {
		iter := strukt.Fields()
		for iter.Next() {
			selector := iter.Selector()

			if iter.IsOptional() || selector.IsDefinition() || selector.PkgPath() != "" || !selector.IsString() {
				continue
			}

			path := []string{iter.Label()}

			if _, err := iter.Value().Struct(); err != nil {
				paths = append(paths, path)
			} else {
				value := iter.Value()
				for _, fieldPath := range collectFieldPaths(&value) {
					paths = append(paths, append(path, fieldPath...))
				}
			}
		}
	}
	return
}
