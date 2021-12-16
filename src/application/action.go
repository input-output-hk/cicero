package application

import (
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
	IsRunnable(*domain.Action) (bool, map[string][]*domain.Fact, error)
}

type actionService struct {
	logger           *log.Logger
	actionRepository repository.ActionRepository
	factRepository   repository.FactRepository
}

func NewActionService(db config.PgxIface, prometheusAddr string) ActionService {
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

// TODO return type => map[string]factOrArrayOfFacts? interface{} directly?
func (self *actionService) IsRunnable(action *domain.Action) (bool, map[string][]*domain.Fact, error) {
	self.logger.Printf("Checking whether Action %s is runnable", action.ID)

	inputs := map[string][]*domain.Fact{}

	// XXX We only check that the required paths are present.
	// In the future we could extend this to also check some values.
	// For that we would have to convert CUE constraints to SQL clauses
	// which sounds non-trivial and it would only help improve performance,
	// except we define a subset of constraints as the only conditions we support
	// and no longer run the CUE filter over the selected facts afterwards.

	facts := struct {
		Latest     map[string]*domain.Fact
		LatestNone map[string]*domain.Fact
		All        map[string][]*domain.Fact
	}{}
	for input, v := range action.Inputs.Latest {
		if fact, err := self.factRepository.GetLatestByFields(collectFieldPaths(v)); err != nil {
			return false, inputs, err
		} else {
			facts.Latest[input] = &fact
		}
	}
	for input, v := range action.Inputs.LatestNone {
		if fact, err := self.factRepository.GetLatestByFields(collectFieldPaths(v)); err != nil {
			return false, inputs, err
		} else {
			facts.LatestNone[input] = &fact
		}
	}
	for input, v := range action.Inputs.All {
		if facts_, err := self.factRepository.GetByFields(collectFieldPaths(v)); err != nil {
			return false, inputs, err
		} else {
			facts.All[input] = facts_
		}
	}

	// TODO nyi
	// check facts filter against all facts

	return true, inputs, nil
}

func collectFieldPaths(value cue.Value) (paths []string) {
	// FIXME support nested paths (x.y, atm just x is found) => return [][]string
	if strukt, err := value.Struct(); err != nil {
		return
	} else {
		iter := strukt.Fields()
		for iter.Next() {
			if iter.IsOptional() || iter.IsDefinition() || iter.IsHidden() {
				continue
			}
			selector := iter.Selector()
			if !selector.IsString() {
				continue
			}
			paths = append(paths, selector.String())
		}
	}
	return
}
