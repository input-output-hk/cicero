package service

import (
	"context"
	"fmt"

	"cuelang.org/go/cue"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type ActionService interface {
	WithQuerier(config.PgxIface) ActionService

	GetById(uuid.UUID) (domain.Action, error)
	GetByRunId(uuid.UUID) (domain.Action, error)
	GetLatestByName(string) (domain.Action, error)
	GetAll() ([]*domain.Action, error)
	GetCurrent() ([]*domain.Action, error)
	Save(*domain.Action) error
	IsRunnable(*domain.Action) (bool, map[string]interface{}, error)
	Create(string, string) (*domain.Action, error)
	Invoke(*domain.Action) (bool, error)
	InvokeCurrent() error
}

type actionService struct {
	logger            zerolog.Logger
	actionRepository  repository.ActionRepository
	factRepository    repository.FactRepository
	evaluationService EvaluationService
	runService        RunService
	nomadClient       application.NomadClient
	db                config.PgxIface
}

func NewActionService(db config.PgxIface, nomadClient application.NomadClient, runService RunService, evaluationService EvaluationService, logger *zerolog.Logger) ActionService {
	return &actionService{
		logger:            logger.With().Str("component", "ActionService").Logger(),
		actionRepository:  persistence.NewActionRepository(db),
		factRepository:    persistence.NewFactRepository(db),
		evaluationService: evaluationService,
		nomadClient:       nomadClient,
		runService:        runService,
		db:                db,
	}
}

func (self *actionService) WithQuerier(querier config.PgxIface) ActionService {
	return &actionService{
		logger:            self.logger,
		actionRepository:  self.actionRepository.WithQuerier(querier),
		factRepository:    self.factRepository.WithQuerier(querier),
		runService:        self.runService.WithQuerier(querier),
		evaluationService: self.evaluationService,
		nomadClient:       self.nomadClient,
		db:                querier,
	}
}

func (self *actionService) GetById(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting Action by ID")
	action, err = self.actionRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	return
}

func (self *actionService) GetByRunId(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Debug().Str("id", id.String()).Msg("Getting Action by Run ID")
	action, err = self.actionRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Run ID %q", id)
	return
}

func (self *actionService) GetLatestByName(name string) (action domain.Action, err error) {
	self.logger.Debug().Str("name", name).Msg("Getting latest Action by name")
	action, err = self.actionRepository.GetLatestByName(name)
	err = errors.WithMessagef(err, "Could not select latest Action for name %q", name)
	return
}

func (self *actionService) GetAll() ([]*domain.Action, error) {
	self.logger.Debug().Msg("Getting all Actions")
	return self.actionRepository.GetAll()
}

func (self *actionService) Save(action *domain.Action) error {
	self.logger.Debug().Str("name", action.Name).Msg("Saving new Action")
	if err := self.actionRepository.Save(action); err != nil {
		return errors.WithMessagef(err, "Could not insert Action")
	}
	self.logger.Debug().Str("id", action.ID.String()).Msg("Created Action")
	return nil
}

func (self *actionService) GetCurrent() (actions []*domain.Action, err error) {
	self.logger.Debug().Msg("Getting current Actions")
	actions, err = self.actionRepository.GetCurrent()
	err = errors.WithMessagef(err, "Could not select current Actions")
	return
}

func (self *actionService) IsRunnable(action *domain.Action) (bool, map[string]interface{}, error) {
	logger := self.logger.With().
		Str("name", action.Name).
		Str("id", action.ID.String()).
		Logger()

	logger.Debug().Msg("Checking whether Action is runnable")

	inputFact := map[string]*domain.Fact{}
	inputFacts := map[string][]*domain.Fact{}

	inputs := map[string]interface{}{}

	// XXX We only check that the required paths are present.
	// In the future we could extend this to also check some values.
	// For that we would have to convert CUE constraints to SQL clauses
	// which sounds non-trivial and it would only help improve performance,
	// except we define a subset of constraints as the only conditions we support
	// and no longer run the CUE filter over the selected facts afterwards.
	// But heads up! This would mean that inputs' `select: "latest"` behavior
	// would be changed, as what is latest currently means "latest with these paths"
	// but would then mean "latest with these paths AND matching values", so beware!

	// Select candidate facts.
	for name, input := range action.Inputs {
		inputLogger := logger.With().Str("input", name).Logger()

		switch input.Select {
		case domain.InputDefinitionSelectLatest:
			switch fact, err := self.getInputFactLatest(input.Match.WithoutInputs()); {
			case err != nil:
				return false, nil, err
			case fact == nil:
				if !input.Not && !input.Optional {
					inputLogger.Debug().
						Bool("runnable", false).
						Msg("No fact found for required input")
					return false, nil, nil
				}
			default:
				inputFact[name] = fact
				if !input.Not {
					inputs[name] = fact
				}
			}
		case domain.InputDefinitionSelectAll:
			switch facts, err := self.getInputFacts(input.Match.WithoutInputs()); {
			case err != nil:
				return false, nil, err
			case len(facts) == 0:
				if !input.Not && !input.Optional {
					inputLogger.Debug().
						Bool("runnable", false).
						Msg("No facts found for required input")
					return false, nil, nil
				}
			default:
				inputFacts[name] = facts
				if !input.Not {
					inputs[name] = facts
				}
			}
		default:
			return false, nil, fmt.Errorf("InputDefinitionSelect with unknown value %d", input.Select)
		}
	}

	// Match candidate facts.
	for name, input := range action.Inputs {
		inputLogger := logger.With().Str("input", name).Logger()

		switch input.Select {
		case domain.InputDefinitionSelectLatest:
			if inputFactEntry, exists := inputFact[name]; exists {
				if match, err := matchFact(input.Match.WithInputs(inputs), inputFactEntry); err != nil {
					return false, nil, err
				} else if match == input.Not {
					if !input.Optional || input.Not {
						inputLogger.Debug().
							Bool("runnable", false).
							Str("fact", inputFactEntry.ID.String()).
							Msg("Fact does not match")
						return false, nil, nil
					}
					delete(inputs, name)
				}
			}
		case domain.InputDefinitionSelectAll:
			if inputFactsEntry, exists := inputFacts[name]; exists {
				for i, fact := range inputFactsEntry {
					if match, err := matchFact(input.Match.WithInputs(inputs), fact); err != nil {
						return false, nil, err
					} else if match == input.Not {
						if !input.Optional || input.Not {
							inputLogger.Debug().
								Bool("runnable", false).
								Str("fact", fact.ID.String()).
								Msg("Fact does not match")
							return false, nil, nil
						}
						if facts, exists := inputs[name]; exists {
							// We will filter `nil`s out later as doing that here would be costly.
							facts.([]*domain.Fact)[i] = nil
						}
					}
				}

				if facts, exists := inputs[name]; exists {
					// Filter out `nil` entries from non-matching facts.
					newFacts := []*domain.Fact{}
					for _, fact := range facts.([]*domain.Fact) {
						if fact == nil {
							continue
						}
						newFacts = append(newFacts, fact)
					}
					inputs[name] = newFacts

					if len(newFacts) == 0 {
						if !input.Optional {
							inputLogger.Debug().
								Bool("runnable", false).
								Msg("No facts match")
							return false, nil, nil
						}
						delete(inputs, name)
					}
				}
			}
		default:
			panic("This should have already been caught by the loop above!")
		}
	}

	// Not runnable if the inputs are the same as last run.
	if run, err := self.runService.GetLatestByActionId(action.ID); err != nil {
		if !pgxscan.NotFound(err) {
			return false, nil, err
		}
	} else {
		var inputFactIds map[string][]uuid.UUID
		if inputFactIds, err = self.runService.GetInputFactIdsByNomadJobId(run.NomadJobID); err != nil {
			if !pgxscan.NotFound(err) {
				return false, nil, err
			}
			inputFactIds = map[string][]uuid.UUID{}
		}

		inputFactsChanged := false

	InputFactsChanged:
		for name, input := range action.Inputs {
			if _, exists := inputs[name]; !exists {
				// We only care about inputs that are
				// passed into the evaluation.
				continue
			}

			didInputFactChange := func(oldFactIdsIndex int, newFact *domain.Fact) bool {
				var oldFactId *uuid.UUID
				if _, hasOldFact := inputFactIds[name]; hasOldFact {
					oldFactId = &inputFactIds[name][oldFactIdsIndex]
				}

				switch {
				case input.Optional && oldFactId == nil:
					if newFact != nil {
						if logger.Debug().Enabled() {
							logger.Debug().
								Str("input", name).
								Bool("input-optional", true).
								Interface("old-fact", nil).
								Str("new-fact", newFact.ID.String()).
								Msg("input satisfied by new Fact")
						}
						return true
					}
				case input.Optional && oldFactId != nil:
					if newFact == nil || *oldFactId != newFact.ID {
						if logger.Debug().Enabled() {
							var newFactIdToLog *string
							if newFact != nil {
								newFactIdToLogValue := newFact.ID.String()
								newFactIdToLog = &newFactIdToLogValue
							}
							logger.Debug().
								Str("input", name).
								Bool("input-optional", true).
								Str("old-fact", oldFactId.String()).
								Interface("new-fact", newFactIdToLog).
								Msg("input satisfied by new Fact or absence of one")
						}
						return true
					}
				case oldFactId == nil:
					// A previous Run would not have been started
					// if a non-optional input was not satisfied.
					// We are not looking at a negated input here
					// because those are never passed into the evaluation.
					panic("This should never happen™")
				case *oldFactId != newFact.ID:
					if logger.Debug().Enabled() {
						logger.Debug().
							Str("input", name).
							Str("old-fact", oldFactId.String()).
							Str("new-fact", newFact.ID.String()).
							Msg("input satisfied by new Fact")
					}
					return true
				}

				return false
			}

			switch input.Select {
			case domain.InputDefinitionSelectLatest:
				if didInputFactChange(0, inputFact[name]) {
					inputFactsChanged = true
				}
			case domain.InputDefinitionSelectAll:
				if newFacts, oldFacts := inputFacts[name], inputFactIds[name]; len(newFacts) != len(oldFacts) {
					logger.Debug().
						Str("input", name).
						Int("num-old-facts", len(oldFacts)).
						Int("num-new-facts", len(newFacts)).
						Msg("input satisfied by different number of Facts than last Run")
					inputFactsChanged = true
				} else {
				LoopOverNewInputFacts:
					for _, match := range newFacts {
						for i := range oldFacts {
							if didInputFactChange(i, match) {
								inputFactsChanged = true
								break LoopOverNewInputFacts
							}
						}
					}
				}
			default:
				panic("This should have already been caught by the loop above!")
			}

			if inputFactsChanged {
				break InputFactsChanged
			}

			logger.Debug().
				Str("input", name).
				Msg("input satisfied by same Fact(s) as last Run")
		}

		if !inputFactsChanged {
			return false, inputs, nil
		}
	}

	// Filter input facts. We only provide keys requested by the CUE expression.
	for name, input := range action.Inputs {
		switch input.Select {
		case domain.InputDefinitionSelectLatest:
			if entry, exists := inputs[name]; exists {
				filterFields(&entry.(*domain.Fact).Value, input.Match.WithoutInputs())
			}
		case domain.InputDefinitionSelectAll:
			if entry, exists := inputs[name]; exists {
				match := input.Match.WithoutInputs()
				for _, fact := range entry.([]*domain.Fact) {
					filterFields(&fact.Value, match)
				}
			}
		default:
			panic("This should have already been caught by the loop above!")
		}
	}

	logger.Debug().Bool("runnable", true).Send()
	return true, inputs, nil
}

func filterFields(factValue *interface{}, filter cue.Value) {
	if strukt, err := filter.Struct(); err != nil {
		if _, factIsMap := (*factValue).(map[string]interface{}); factIsMap {
			// fact is not allowed to be map because filter is not a struct
			*factValue = nil
		}
	} else if value, factIsMap := (*factValue).(map[string]interface{}); factIsMap {
		for k, v := range value {
			if field, err := strukt.FieldByName(k, false); err != nil {
				delete(value, k)
			} else {
				filterFields(&v, field.Value)
				value[k] = v
			}
		}
	} else {
		// fact must be a map because filter is struct
		*factValue = map[string]interface{}{}
	}
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

func (self *actionService) Create(source, name string) (*domain.Action, error) {
	action := domain.Action{
		ID:     uuid.New(),
		Name:   name,
		Source: source,
	}

	var actionDef domain.ActionDefinition
	if def, err := self.evaluationService.EvaluateAction(source, name, action.ID); err != nil {
		self.logger.Err(err).Send()
		return nil, err
	} else {
		actionDef = def
	}

	action.Meta = actionDef.Meta
	action.Inputs = actionDef.Inputs

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		if err := txSelf.Save(&action); err != nil {
			return err
		}

		_, err := txSelf.Invoke(&action)

		return err
	}); err != nil {
		return nil, err
	}

	return &action, nil
}

func (self *actionService) Invoke(action *domain.Action) (bool, error) {
	runnable := false
	return runnable, self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		runnable_, inputs, err := txSelf.IsRunnable(action)
		runnable = runnable_
		if err != nil || !runnable {
			return err
		}

		runDef, err := self.evaluationService.EvaluateRun(action.Source, action.Name, action.ID, inputs)
		if err != nil {
			var evalErr EvaluationError
			if errors.As(err, &evalErr) {
				self.logger.Err(evalErr).
					Str("source", action.Source).
					Str("name", action.Name).
					Msg("Could not evaluate action")
			}
			return err
		}

		run := domain.Run{
			ActionId: action.ID,
		}

		if err := self.runService.WithQuerier(tx).Save(&run, inputs, &runDef.Output); err != nil {
			return errors.WithMessage(err, "Could not insert Run")
		}

		if runDef.IsDecision() {
			if runDef.Output.Success != nil {
				if err := self.factRepository.WithQuerier(tx).Save(&domain.Fact{Value: runDef.Output.Success}, nil); err != nil {
					return errors.WithMessage(err, "Could not publish fact")
				}
			}

			run.CreatedAt = run.CreatedAt.UTC()
			run.FinishedAt = &run.CreatedAt

			err := self.runService.WithQuerier(tx).Update(&run)
			err = errors.WithMessage(err, "Could not update decision Run")

			return err
		}

		runId := run.NomadJobID.String()
		runDef.Job.ID = &runId

		if response, _, err := self.nomadClient.JobsRegister(runDef.Job, &nomad.WriteOptions{}); err != nil {
			return errors.WithMessage(err, "Failed to run Action")
		} else if len(response.Warnings) > 0 {
			self.logger.Warn().
				Str("nomad-job", runId).
				Str("nomad-evaluation", response.EvalID).
				Str("warnings", response.Warnings).
				Msg("Warnings occured registering Nomad job")
		}

		return nil
	})
}

func (self *actionService) InvokeCurrent() error {
	return self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		actions, err := txSelf.GetCurrent()
		if err != nil {
			return err
		}

		for {
			anyRunnable := false

			for _, action := range actions {
				runnable, err := txSelf.Invoke(action)
				if err != nil {
					return err
				}

				anyRunnable = anyRunnable || runnable
			}

			if !anyRunnable {
				break
			}
		}

		return nil
	})
}
