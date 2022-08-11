package service

import (
	"context"
	"sync"

	"cuelang.org/go/cue"
	cueliteral "cuelang.org/go/cue/literal"
	"cuelang.org/go/tools/flow"
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
	withQuerier(config.PgxIface, ActionServiceCyclicDependencies) ActionService

	GetById(uuid.UUID) (*domain.Action, error)
	GetByInvocationId(uuid.UUID) (*domain.Action, error)
	GetByRunId(uuid.UUID) (*domain.Action, error)
	GetByName(string, *repository.Page) ([]domain.Action, error)
	GetLatestByName(string) (*domain.Action, error)
	GetAll() ([]domain.Action, error)
	GetCurrent() ([]domain.Action, error)
	GetCurrentActive() ([]domain.Action, error)
	Save(*domain.Action) error
	Update(*domain.Action) error
	GetSatisfiedInputs(*domain.Action) (map[string]domain.Fact, map[string]cue.Value, bool, error)
	IsRunnable(*domain.Action) (bool, map[string]domain.Fact, error)
	Create(string, string) (*domain.Action, error)
	// Returns a nil pointer for the first return value if the Action was not runnable.
	Invoke(*domain.Action) (*domain.Invocation, InvokeRunFunc, error)
	InvokeCurrentActive() (func(config.PgxIface) (InvokeRegisterFunc, error), error)
}

type InvokeRunFunc func(config.PgxIface) (*domain.Run, InvokeRegisterFunc, error)
type InvokeRegisterFunc func() error

type ActionServiceCyclicDependencies struct {
	invocationService *InvocationService
	factService       *FactService
}

type actionService struct {
	logger            zerolog.Logger
	actionRepository  repository.ActionRepository
	evaluationService EvaluationService
	runService        RunService
	nomadClient       application.NomadClient
	db                config.PgxIface
	ActionServiceCyclicDependencies
}

func NewActionService(db config.PgxIface, nomadClient application.NomadClient, invocationService *InvocationService, factService *FactService, runService RunService, evaluationService EvaluationService, logger *zerolog.Logger) ActionService {
	return &actionService{
		logger:            logger.With().Str("component", "ActionService").Logger(),
		actionRepository:  persistence.NewActionRepository(db),
		evaluationService: evaluationService,
		nomadClient:       nomadClient,
		runService:        runService,
		db:                db,
		ActionServiceCyclicDependencies: ActionServiceCyclicDependencies{
			invocationService: invocationService,
			factService:       factService,
		},
	}
}

func (self actionService) WithQuerier(querier config.PgxIface) ActionService {
	return self.withQuerier(querier, ActionServiceCyclicDependencies{})
}

func (self actionService) withQuerier(querier config.PgxIface, cyclicDeps ActionServiceCyclicDependencies) ActionService {
	result := actionService{
		logger:                          self.logger,
		actionRepository:                self.actionRepository.WithQuerier(querier),
		runService:                      self.runService.WithQuerier(querier),
		evaluationService:               self.evaluationService,
		nomadClient:                     self.nomadClient,
		db:                              querier,
		ActionServiceCyclicDependencies: cyclicDeps,
	}

	if result.invocationService == nil {
		r := ActionService(result)
		result.invocationService = new(InvocationService)
		*result.invocationService = (*self.invocationService).withQuerier(querier, InvocationServiceCyclicDependencies{actionService: &r})
	}
	if result.factService == nil {
		r := ActionService(result)
		result.factService = new(FactService)
		*result.factService = (*self.factService).withQuerier(querier, FactServiceCyclicDependencies{
			actionService: &r,
		})
	}

	return &result
}

func (self actionService) GetById(id uuid.UUID) (action *domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by ID")
	action, err = self.actionRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	return
}

func (self actionService) GetByRunId(id uuid.UUID) (action *domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by Run ID")
	action, err = self.actionRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Run ID %q", id)
	return
}

func (self actionService) GetByInvocationId(id uuid.UUID) (action *domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by Invocation ID")
	action, err = self.actionRepository.GetByInvocationId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Invocation ID %q", id)
	return
}

func (self actionService) GetByName(name string, page *repository.Page) (actions []domain.Action, err error) {
	self.logger.Trace().Str("name", name).Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting Actions by name")
	actions, err = self.actionRepository.GetByName(name, page)
	err = errors.WithMessagef(err, "Could not select Actions for name %q with offset %d and limit %d", name, page.Offset, page.Limit)
	return
}

func (self actionService) GetLatestByName(name string) (action *domain.Action, err error) {
	self.logger.Trace().Str("name", name).Msg("Getting latest Action by name")
	action, err = self.actionRepository.GetLatestByName(name)
	err = errors.WithMessagef(err, "Could not select latest Action for name %q", name)
	return
}

func (self actionService) GetAll() ([]domain.Action, error) {
	self.logger.Trace().Msg("Getting all Actions")
	return self.actionRepository.GetAll()
}

func (self actionService) Save(action *domain.Action) error {
	self.logger.Trace().Str("name", action.Name).Msg("Saving new Action")
	if err := self.actionRepository.Save(action); err != nil {
		return errors.WithMessagef(err, "Could not insert Action")
	}
	self.logger.Trace().Str("id", action.ID.String()).Msg("Created Action")
	return nil
}

func (self actionService) Update(action *domain.Action) error {
	self.logger.Trace().Str("id", action.ID.String()).Msg("Updating Action")
	if err := self.actionRepository.Update(action); err != nil {
		return errors.WithMessagef(err, "Could not update Action")
	}
	self.logger.Trace().Str("id", action.ID.String()).Msg("Updated Action")
	return nil
}

func (self actionService) GetCurrent() (actions []domain.Action, err error) {
	self.logger.Trace().Msg("Getting current Actions")
	actions, err = self.actionRepository.GetCurrent()
	err = errors.WithMessagef(err, "Could not select current Actions")
	return
}

func (self actionService) GetCurrentActive() (actions []domain.Action, err error) {
	self.logger.Trace().Msg("Getting current active Actions")
	actions, err = self.actionRepository.GetCurrentActive()
	err = errors.WithMessagef(err, "Could not select current active Actions")
	return
}

func (self actionService) GetSatisfiedInputs(action *domain.Action) (map[string]domain.Fact, map[string]cue.Value, bool, error) {
	logger := self.logger.With().
		Str("name", action.Name).
		Str("id", action.ID.String()).
		Logger()

	inputs := map[string]domain.Fact{}
	inputMatchWithDeps := map[string]cue.Value{}

	dbConnMutex := &sync.Mutex{}
	valuePath := cue.MakePath(cue.Str("value"))

	errNotRunnable := errors.New("not runnable")
	if flow, err := action.InOut.InputsFlow(func(t *flow.Task) error {
		name := t.Path().Selectors()[1].String() // inputs: <name>: …
		if name_, err := cueliteral.Unquote(name); err == nil {
			name = name_
		}
		input, err := action.InOut.Input(name, nil)
		if err != nil {
			return err
		}
		tValue := t.Value().LookupPath(valuePath)
		inputLogger := logger.With().Str("input", name).Logger()

		// A transaction happens on exactly one connection so
		// we cannot use more connections to run queries in parallel.
		dbConnMutex.Lock()
		defer dbConnMutex.Unlock()

		switch fact, err := (*self.factService).GetLatestByCue(tValue); {
		case err != nil:
			return err
		case fact == nil:
			if !input.Not && !input.Optional {
				inputLogger.Debug().
					Bool("runnable", false).
					Msg("No fact found for required input")
				return errNotRunnable
			}
		default:
			if !input.Not {
				inputs[name] = *fact
			}

			// Match candidate fact.
			var match cue.Value
			// XXX Is `action.InOut.Input(name, inputs).Match` the same as `tValue`?
			if inputWithDeps, err := action.InOut.Input(name, inputs); err != nil {
				return err
			} else {
				match = inputWithDeps.Match
			}
			inputMatchWithDeps[name] = match.Eval()
			if _, matchErr, err := (*self.factService).Match(fact, match); err != nil {
				return err
			} else {
				switch {
				case matchErr == nil && input.Not:
					inputLogger.Debug().
						Bool("runnable", false).
						Str("fact", fact.ID.String()).
						Msg("Fact matches negated input")
					delete(inputs, name)
					return errNotRunnable
				case matchErr != nil && !input.Not && !input.Optional:
					inputLogger.Debug().
						Bool("runnable", false).
						Str("fact", fact.ID.String()).
						AnErr("mismatch", matchErr).
						Msg("Fact does not match required input")
					delete(inputs, name)
					return errNotRunnable
				}
			}

			// Fill the CUE expression with the fact's value
			// so it can be referenced by its dependents.
			if _, exists := inputs[name]; exists {
				if err := t.Fill(struct {
					Value interface{} `json:"value"`
				}{fact.Value}); err != nil {
					return err
				}
			}
		}

		return nil
	}); err != nil {
		return nil, nil, false, err
	} else if flow.Run(context.Background()); err != nil {
		if errors.Is(err, errNotRunnable) {
			return inputs, inputMatchWithDeps, false, nil
		}
		return nil, nil, false, err
	}

	return inputs, inputMatchWithDeps, true, nil
}

func (self actionService) IsRunnable(action *domain.Action) (bool, map[string]domain.Fact, error) {
	logger := self.logger.With().
		Str("name", action.Name).
		Str("id", action.ID.String()).
		Logger()

	logger.Debug().Msg("Checking whether Action is runnable")

	// Select and match candidate facts.
	var inputs map[string]domain.Fact
	switch inputs_, _, maybeRunnable, err := self.GetSatisfiedInputs(action); {
	case err != nil:
		return false, nil, err
	case !maybeRunnable:
		return false, inputs_, nil
	default:
		inputs = inputs_
	}

	// Not runnable if the inputs are the same as last invocation.
	if invocation, err := (*self.invocationService).GetLatestByActionId(action.ID); err != nil {
		return false, nil, err
	} else if invocation != nil {
		var inputFactIds map[string]uuid.UUID
		if inputFactIds, err = (*self.invocationService).GetInputFactIdsById(invocation.Id); err != nil {
			return false, nil, err
		}

		inputFactsChanged := false

		if inputsWithDeps, err := action.InOut.Inputs(inputs); err != nil {
			return false, nil, err
		} else {
		InputFactsChanged:
			for name, input := range inputsWithDeps {
				if _, exists := inputs[name]; !exists {
					// We only care about inputs that are
					// passed into the evaluation.
					continue
				}

				var oldFactId *uuid.UUID
				if _, hasOldFact := inputFactIds[name]; hasOldFact {
					var oldFactIdOnHeap = inputFactIds[name]
					oldFactId = &oldFactIdOnHeap
				}

				switch {
				case input.Optional && oldFactId == nil:
					if _, exists := inputs[name]; exists {
						if logger.Debug().Enabled() {
							logger.Debug().
								Str("input", name).
								Bool("input-optional", true).
								Interface("old-fact", nil).
								Str("new-fact", inputs[name].ID.String()).
								Msg("input satisfied by new Fact")
						}
						inputFactsChanged = true
						break InputFactsChanged
					}
				case input.Optional && oldFactId != nil:
					if _, exists := inputs[name]; !exists || *oldFactId != inputs[name].ID {
						if logger.Debug().Enabled() {
							var newFactIdToLog *string
							if !exists {
								newFactIdToLogValue := inputs[name].ID.String()
								newFactIdToLog = &newFactIdToLogValue
							}
							logger.Debug().
								Str("input", name).
								Bool("input-optional", true).
								Str("old-fact", oldFactId.String()).
								Interface("new-fact", newFactIdToLog).
								Msg("input satisfied by new Fact or absence of one")
						}
						inputFactsChanged = true
						break InputFactsChanged
					}
				case oldFactId == nil:
					// A previous Invocation would not have been done
					// if a non-optional input was not satisfied.
					// We are not looking at a negated input here
					// because those are never passed into the evaluation.
					panic("This should never happen™")
				case *oldFactId != inputs[name].ID:
					if logger.Debug().Enabled() {
						logger.Debug().
							Str("input", name).
							Str("old-fact", oldFactId.String()).
							Str("new-fact", inputs[name].ID.String()).
							Msg("input satisfied by new Fact")
					}
					inputFactsChanged = true
					break InputFactsChanged
				}

				logger.Debug().
					Str("input", name).
					Msg("input satisfied by same Fact(s) as last Invocation")
			}
		}

		if !inputFactsChanged {
			return false, inputs, nil
		}
	}

	// Filter input facts. We only provide keys requested by the CUE expression.
	/* FIXME Removes everything if CUE's builtin `or` function is used.
	for name, input := range action.InOut.Inputs(inputs) {
		if entry, exists := inputs[name]; exists {
			filterFields(&entry.Value, input.Match)
		}
	}
	*/

	logger.Debug().Bool("runnable", true).Send()
	return true, inputs, nil
}

//nolint:unused
func filterFields(factValue *interface{}, filter cue.Value) {
	if value, factIsMap := (*factValue).(map[string]interface{}); factIsMap {
		if filter.Kind().IsAnyOf(cue.StructKind) {
			for k, v := range value {
				if filterV := filter.LookupPath(cue.MakePath(cue.Str(k)).Optional()); filterV.Exists() {
					filterFields(&v, filterV)
					value[k] = v
				} else {
					delete(value, k)
				}
			}
		} else {
			// fact is not allowed to be map because filter is not allowed to be a struct
			*factValue = nil
		}
	} else if filter.Kind().IsAnyOf(cue.StructKind) {
		// fact must be a map because filter is struct
		*factValue = map[string]interface{}{}
	}
}

func (self actionService) Create(source, name string) (*domain.Action, error) {
	action := domain.Action{
		ID:     uuid.New(),
		Name:   name,
		Source: source,
		Active: true,
	}

	if def, err := self.evaluationService.EvaluateAction(source, name, action.ID); err != nil {
		self.logger.Err(err).Send()
		return nil, err
	} else {
		action.ActionDefinition = def
	}

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*actionService)

		// deactivate previous version for convenience
		if prev, err := txSelf.GetLatestByName(action.Name); err != nil {
			return err
		} else if prev != nil && prev.Active {
			prev.Active = false
			if err := txSelf.Update(prev); err != nil {
				return err
			}
		}

		return txSelf.Save(&action)
	}); err != nil {
		return nil, err
	}

	return &action, nil
}

func (self actionService) Invoke(action *domain.Action) (*domain.Invocation, InvokeRunFunc, error) {
	var invocation *domain.Invocation
	var inputs map[string]domain.Fact
	var runnable bool

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*actionService)

		if runnable_, inputs_, err := txSelf.IsRunnable(action); err != nil {
			return err
		} else {
			runnable = runnable_
			inputs = inputs_
		}

		if !runnable {
			return nil
		}

		invocation = &domain.Invocation{ActionId: action.ID}
		if err := (*txSelf.invocationService).Save(invocation, inputs); err != nil {
			return err
		}

		return nil
	}); err != nil {
		return invocation, nil, err
	}

	if !runnable {
		return nil, nil, nil
	}

	return invocation, func(db config.PgxIface) (*domain.Run, InvokeRegisterFunc, error) {
		job, err := self.evaluationService.EvaluateRun(action.Source, action.Name, action.ID, invocation.Id, inputs)
		if err != nil {
			if err := (*self.invocationService).WithQuerier(db).End(invocation.Id); err != nil {
				return nil, nil, err
			}

			var evalErr *EvaluationError
			if errors.As(err, &evalErr) {
				return nil, nil, nil
			}
			return nil, nil, err
		}

		var run *domain.Run
		var registerFunc InvokeRegisterFunc

		if err := db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			txSelf := self.WithQuerier(tx).(*actionService)

			if err := (*txSelf.invocationService).End(invocation.Id); err != nil {
				return err
			}

			tmpRun := domain.Run{
				InvocationId: invocation.Id,
				Status:       domain.RunStatusRunning,
			}

			if err := txSelf.runService.Save(&tmpRun); err != nil {
				return errors.WithMessage(err, "Could not insert Run")
			}

			if job == nil { // An action that has no job is called a decision.
				if output, err := action.InOut.Output(inputs); err != nil {
					return err
				} else if success := output.Success; success.Exists() {
					fact := domain.Fact{
						RunId: &tmpRun.NomadJobID,
						Value: success,
					}
					if err := (*txSelf.factService).Save(&fact, nil); err != nil {
						return errors.WithMessage(err, "Could not publish fact")
					}
					if runFunc, err := txSelf.InvokeCurrentActive(); err != nil {
						return err
					} else if registerFunc, err = runFunc(tx); err != nil {
						return err
					}
				}

				tmpRun.CreatedAt = tmpRun.CreatedAt.UTC()
				tmpRun.FinishedAt = &tmpRun.CreatedAt
				tmpRun.Status = domain.RunStatusSucceeded

				err := txSelf.runService.Update(&tmpRun)
				err = errors.WithMessage(err, "Could not update decision Run")

				run = &tmpRun
				return err
			}

			runId := tmpRun.NomadJobID.String()
			job.ID = &runId

			run = &tmpRun
			registerFunc = func() error {
				if response, _, err := self.nomadClient.JobsRegister(job, &nomad.WriteOptions{}); err != nil {
					return errors.WithMessage(err, "Failed to run Action")
				} else if len(response.Warnings) > 0 {
					self.logger.Warn().
						Str("nomad-job", runId).
						Str("nomad-evaluation", response.EvalID).
						Str("warnings", response.Warnings).
						Msg("Warnings occured registering Nomad job")
				}
				return nil
			}

			return nil
		}); err != nil {
			if err := (*self.invocationService).WithQuerier(db).End(invocation.Id); err != nil {
				return run, registerFunc, err
			}

			return run, registerFunc, err
		}

		return run, registerFunc, nil
	}, nil
}

func (self actionService) InvokeCurrentActive() (func(config.PgxIface) (InvokeRegisterFunc, error), error) {
	runFuncs := []InvokeRunFunc{}
	return func(db config.PgxIface) (InvokeRegisterFunc, error) {
			registerFuncs := []InvokeRegisterFunc{}

			for _, runFunc := range runFuncs {
				if _, registerFunc, err := runFunc(db); err != nil {
					return nil, err
				} else if registerFunc != nil {
					registerFuncs = append(registerFuncs, registerFunc)
				}
			}

			return func() error {
				for _, registerFunc := range registerFuncs {
					if err := registerFunc(); err != nil {
						return err
					}
				}
				return nil
			}, nil
		}, self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			txSelf := self.WithQuerier(tx).(*actionService)

			actions, err := txSelf.GetCurrentActive()
			if err != nil {
				return err
			}

			for {
				anyRunnable := false

				for _, action := range actions {
					// copy so we don't point to loop variable
					action := action
					if invocation, runFunc, err := txSelf.Invoke(&action); err != nil {
						return err
					} else {
						anyRunnable = anyRunnable || invocation != nil
						if runFunc != nil {
							runFuncs = append(runFuncs, runFunc)
						}
					}
				}

				if !anyRunnable {
					break
				}
			}

			return nil
		})
}
