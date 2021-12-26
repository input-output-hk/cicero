package service

import (
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
	err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	return
}

func (self *actionService) GetLatestByName(name string) (action domain.Action, err error) {
	self.logger.Printf("Getting latest Action by name %s", name)
	action, err = self.actionRepository.GetLatestByName(name)
	err = errors.WithMessagef(err, "Could not select latest Action for name %q", name)
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
	err = errors.WithMessagef(err, "Could not select current Actions")
	return
}

func (self *actionService) IsRunnable(action *domain.Action) (bool, map[string]interface{}, error) {
	self.logger.Printf("Checking whether Action %s is runnable", action.ID)

	inputFact := map[string]*domain.Fact{}
	inputFacts := map[string][]*domain.Fact{}

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
			if fact, err := self.getInputFactLatest(input.Match.WithoutInputs()); err != nil {
				return false, nil, err
			} else if fact != nil {
				inputFact[name] = fact
				if !input.Not {
					inputs[name] = fact
				}
			}
		case domain.InputDefinitionSelectAll:
			if facts, err := self.getInputFacts(input.Match.WithoutInputs()); err != nil {
				return false, nil, err
			} else if len(facts) > 0 {
				inputFacts[name] = facts
				if !input.Not {
					inputs[name] = facts
				}
			}
		default:
			return false, nil, fmt.Errorf("InputDefinitionSelect with unknown value %d", input.Select)
		}
	}

	for name, input := range action.Inputs {
		switch input.Select {
		case domain.InputDefinitionSelectLatest:
			if inputFactEntry, exists := inputFact[name]; exists {
				if match, err := matchFact(input.Match.WithInputs(inputs), inputFactEntry); err != nil {
					return false, inputs, err
				} else if match == input.Not {
					return false, inputs, nil
				}
			} else if !input.Not && !input.Optional {
				return false, inputs, nil
			}
		case domain.InputDefinitionSelectAll:
			if inputFactsEntry, exists := inputFacts[name]; exists {
				for _, fact := range inputFactsEntry {
					if match, err := matchFact(input.Match.WithInputs(inputs), fact); err != nil {
						return false, inputs, err
					} else if match == input.Not {
						return false, inputs, nil
					}
				}
			} else if !input.Not && !input.Optional {
				return false, inputs, nil
			}
		default:
			panic("This should have already been caught by the loop above!")
		}
	}

	return true, inputs, nil
}

func (self *actionService) getInputFactLatest(value cue.Value) (*domain.Fact, error) {
	fact, err := self.factRepository.GetLatestByFields(collectFieldPaths(value))
	if err != nil && errors.Is(err, pgx.ErrNoRows) {
		return nil, nil
	}
	return &fact, err
}

func (self *actionService) getInputFacts(value cue.Value) ([]*domain.Fact, error) {
	facts, err := self.factRepository.GetByFields(collectFieldPaths(value))
	if err != nil && errors.Is(err, pgx.ErrNoRows) {
		return nil, nil
	}
	return facts, err
}

func matchFact(match cue.Value, fact *domain.Fact) (bool, error) {
	factCue := match.Context().Encode(fact.Value)
	if err := factCue.Err(); err != nil {
		return false, err
	}

	return match.Subsume(factCue, cue.Final()) == nil, nil
}

func collectFieldPaths(value cue.Value) (paths [][]string) {
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
				for _, fieldPath := range collectFieldPaths(value) {
					paths = append(paths, append(path, fieldPath...))
				}
			}
		}
	}
	return
}
