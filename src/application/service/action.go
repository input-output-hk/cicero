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
	GetCurrentByActive(bool) ([]domain.Action, error)
	Save(*domain.Action) error
	SetActive(string, bool) error
	GetSatisfiedInputs(*domain.Action) (map[string]domain.Fact, error)
	IsRunnable(*domain.Action) (bool, map[string]domain.Fact, error)
	Create(string, string) (*domain.Action, error)
	UpdateSatisfaction(*domain.Fact) error
	// Returns a nil pointer for the first return value if the Action was not runnable.
	Invoke(*domain.Action) (*domain.Invocation, InvokeRunFunc, error)
	InvokeCurrentActive() ([]domain.Invocation, InvokeRunFunc, error)
	NewInvokeRunFunc(*domain.Action, *domain.Invocation, map[string]domain.Fact) InvokeRunFunc
}

// Evaluates the run definition and ends the invocation.
// might return multiple runs in case this was a decision action
// (which success output is always immediately published),
// that caused multiple actions to get invoked.
type InvokeRunFunc func(config.PgxIface) ([]domain.Run, InvokeRegisterFunc, error)

// Registers the nomad job.
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
	self.logger.Trace().Stringer("id", id).Msg("Getting Action by ID")
	action, err = self.actionRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	return
}

func (self actionService) GetByRunId(id uuid.UUID) (action *domain.Action, err error) {
	self.logger.Trace().Stringer("id", id).Msg("Getting Action by Run ID")
	action, err = self.actionRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Run ID %q", id)
	return
}

func (self actionService) GetByInvocationId(id uuid.UUID) (action *domain.Action, err error) {
	self.logger.Trace().Stringer("id", id).Msg("Getting Action by Invocation ID")
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
	logger := self.logger.With().Str("name", action.Name).Logger()
	logger.Trace().Msg("Saving new Action")
	if err := self.actionRepository.Save(action); err != nil {
		return errors.WithMessage(err, "Could not insert Action")
	}
	logger.Trace().Stringer("id", action.ID).Msg("Created Action")
	return nil
}

func (self actionService) SetActive(name string, active bool) error {
	logger := self.logger.With().Str("name", name).Bool("active", active).Logger()
	logger.Trace().Msg("Setting action active status")
	if err := self.actionRepository.SetActive(name, active); err != nil {
		return errors.WithMessage(err, "Could not set action active status")
	}
	logger.Trace().Msg("Set action active status")
	return nil
}

func (self actionService) GetCurrent() (actions []domain.Action, err error) {
	self.logger.Trace().Msg("Getting current Actions")
	actions, err = self.actionRepository.GetCurrent()
	err = errors.WithMessage(err, "Could not select current Actions")
	return
}

func (self actionService) GetCurrentByActive(active bool) (actions []domain.Action, err error) {
	self.logger.Trace().Bool("active", active).Msg("Getting current Actions")
	actions, err = self.actionRepository.GetCurrentByActive(active)
	err = errors.WithMessagef(err, "Could not select current Actions by active %v", active)
	return
}

func (self actionService) GetSatisfiedInputs(action *domain.Action) (inputs map[string]domain.Fact, err error) {
	self.logger.Trace().Stringer("id", action.ID).Msg("Getting satisfactions")
	ids, err := self.actionRepository.GetSatisfactions(action.ID)
	if err != nil {
		err = errors.WithMessage(err, "Could not select satisfactions")
		return
	}
	inputs, err = (*self.factService).GetByIds(ids)
	err = errors.WithMessage(err, "Could not select satisfying facts")
	return
}

func (self actionService) IsRunnable(action *domain.Action) (bool, map[string]domain.Fact, error) {
	logger := self.logger.With().
		Str("name", action.Name).
		Stringer("id", action.ID).
		Logger()

	logger.Debug().Msg("Checking whether Action is runnable")

	inputs, err := self.GetSatisfiedInputs(action)
	if err != nil {
		return false, nil, err
	}

	inputDefs, err := action.InOut.Inputs(nil)
	if err != nil {
		return false, inputs, err
	}

	for name, input := range inputDefs {
		inputLogger := logger.With().Str("input", name).Logger()

		fact, satisfied := inputs[name]

		switch {
		case satisfied && input.Not:
			inputLogger.Debug().
				Bool("runnable", false).
				Stringer("fact", fact.ID).
				Msg("Fact matches negated input")
			return false, inputs, nil
		case !satisfied && !input.Not && !input.Optional:
			inputLogger.Debug().
				Bool("runnable", false).
				Stringer("fact", fact.ID).
				Msg("Required input not satisfied")
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
	}

	if def, err := self.evaluationService.EvaluateAction(source, name, action.ID); err != nil {
		self.logger.Err(err).Send()
		return nil, err
	} else {
		action.ActionDefinition = def
	}

	if err := self.Save(&action); err != nil {
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

		if err := txSelf.actionRepository.DeleteSatisfactions(action.ID); err != nil {
			return err
		}

		return nil
	}); err != nil {
		return invocation, nil, err
	}

	if !runnable {
		return nil, nil, nil
	}

	return invocation, self.NewInvokeRunFunc(action, invocation, inputs), nil
}

func (self actionService) InvokeCurrentActive() ([]domain.Invocation, InvokeRunFunc, error) {
	runFuncs := []InvokeRunFunc{}
	invocations := []domain.Invocation{}
	return invocations, func(db config.PgxIface) ([]domain.Run, InvokeRegisterFunc, error) {
			registerFuncs := make([]InvokeRegisterFunc, 0, len(runFuncs))
			runs := make([]domain.Run, 0, len(runFuncs))

			for _, runFunc := range runFuncs {
				if run, registerFunc, err := runFunc(db); err != nil {
					return nil, nil, err
				} else {
					runs = append(runs, run...)
					registerFuncs = append(registerFuncs, registerFunc)
				}
			}

			return runs, func() error {
				for _, registerFunc := range registerFuncs {
					if err := registerFunc(); err != nil {
						return err
					}
				}
				return nil
			}, nil
		}, self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			txSelf := self.WithQuerier(tx).(*actionService)

			actions, err := txSelf.GetCurrentByActive(true)
			if err != nil {
				return err
			}

			// We need to try to invoke all actions until none was runnable
			// because some might be decision actions that immediately publish a fact,
			// which in turn could make other actions runnable.
			for {
				anyRunnable := false

				for _, action := range actions {
					// copy so we don't point to loop variable
					action := action
					if invocation, runFunc, err := txSelf.Invoke(&action); err != nil {
						return err
					} else {
						anyRunnable = anyRunnable || invocation != nil
						if invocation != nil {
							invocations = append(invocations, *invocation)
						}
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

func (self actionService) NewInvokeRunFunc(action *domain.Action, invocation *domain.Invocation, inputs map[string]domain.Fact) InvokeRunFunc {
	return func(db config.PgxIface) ([]domain.Run, InvokeRegisterFunc, error) {
		job, err := self.evaluationService.EvaluateRun(action.Source, action.Name, action.ID, invocation.Id, inputs)
		if err != nil {
			// XXX Nil pointer when directly calling `(*self.invocationService).WithQuerier(db)` here. Maybe a bug?
			txSelf := self.WithQuerier(db).(*actionService)
			if err := (*txSelf.invocationService).End(invocation.Id); err != nil {
				return nil, nil, err
			}

			// Do not return an EvaluationError so that the transaction commits and the invocation is ended.
			var evalErr *EvaluationError
			if errors.As(err, &evalErr) {
				// Return a dummy registerFunc because we are not returning an error
				// so the caller expects to be able to call it.
				return nil, func() error { return nil }, nil
			}
			return nil, nil, err
		}

		var runs []domain.Run
		var registerFunc InvokeRegisterFunc

		if err := db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
			txSelf := self.WithQuerier(tx).(*actionService)

			if err := (*txSelf.invocationService).End(invocation.Id); err != nil {
				return err
			}

			run := domain.Run{
				InvocationId: invocation.Id,
				Status:       domain.RunStatusRunning,
			}

			if err := txSelf.runService.Save(&run); err != nil {
				return errors.WithMessage(err, "Could not insert Run")
			}

			if job == nil { // An action that has no job is called a decision.
				if success := action.InOut.Output(inputs).Success; success.Exists() {
					fact := domain.Fact{
						RunId: &run.NomadJobID,
						Value: success,
					}
					if _, runFunc, err := (*txSelf.factService).Save(&fact, nil); err != nil {
						return errors.WithMessage(err, "Could not publish fact")
					} else if decisionRuns, registerFunc_, err := runFunc(tx); err != nil {
						return err
					} else {
						registerFunc = registerFunc_
						runs = append(runs, decisionRuns...)
					}
				}

				run.CreatedAt = run.CreatedAt.UTC()
				run.FinishedAt = &run.CreatedAt
				run.Status = domain.RunStatusSucceeded

				err := txSelf.runService.Update(&run)
				err = errors.WithMessage(err, "Could not update decision Run")

				runs = append(runs, run)
				return err
			}

			runId := run.NomadJobID.String()
			job.ID = &runId

			runs = append(runs, run)
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
			if err2 := (*self.invocationService).WithQuerier(db).End(invocation.Id); err2 != nil {
				return runs, registerFunc, errors.WithMessagef(err2, "While ending invocation due to error %q", err.Error())
			}

			return runs, registerFunc, err
		}

		return runs, registerFunc, nil
	}
}

func (self actionService) UpdateSatisfaction(fact *domain.Fact) error {
	return self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*actionService)

		actions, err := txSelf.GetCurrent()
		if err != nil {
			return err
		}

		for _, action := range actions {
			if err := txSelf.updateSatisfaction(&action, fact); err != nil {
				return err
			}
		}

		return nil
	})
}

func (self actionService) updateSatisfaction(action *domain.Action, fact *domain.Fact) error {
	logger := self.logger.With().
		Str("name", action.Name).
		Stringer("id", action.ID).
		Stringer("fact", fact.ID).
		Logger()

	logger.Trace().Msg("Updating satisfaction with new fact")

	return self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx).(*actionService)

		inputs, err := txSelf.GetSatisfiedInputs(action)
		if err != nil {
			return err
		}

		// A transaction happens on exactly one connection so
		// we cannot use more connections to run queries in parallel.
		dbConnMutex := &sync.Mutex{}

		matchPath := cue.MakePath(cue.Str("match"))

		if flow, err := action.InOut.InputsFlow(func(t *flow.Task) error {
			name := t.Path().Selectors()[1].String() // inputs: <name>: …
			if name_, err := cueliteral.Unquote(name); err == nil {
				name = name_
			}

			tMatch := t.Value().LookupPath(matchPath)

			inputLogger := logger.With().Str("input", name).Logger()
			inputLogger.Trace().Msg("Matching")

			//nolint:gocritic // IMHO if-else chain is better than switch here
			if matchErr, err := (*txSelf.factService).Match(fact, tMatch); err != nil {
				return errors.WithMessagef(err, "Could not match fact %s against input %q of action %s", fact.ID, name, action.ID)
			} else if matchErr == nil {
				inputLogger.Trace().Msg("Satisfied")

				inputs[name] = *fact

				dbConnMutex.Lock()
				defer dbConnMutex.Unlock()

				if err := txSelf.actionRepository.SaveSatisfaction(action.ID, name, fact.ID); err != nil {
					return errors.WithMessagef(err, "Could not save satisfaction of input %q on action %q by fact %q", name, action.ID, fact.ID)
				}
			} else {
				inputLogger.Trace().AnErr("match-error", matchErr).Msg("Does not satisfy")

				if oldMatch, exists := inputs[name]; exists {
					// We need to check the old match again in case a dependency changed.
					if matchErr, err := (*txSelf.factService).Match(&oldMatch, tMatch); err != nil {
						return err
					} else if matchErr != nil {
						inputLogger.Trace().Stringer("old-fact", oldMatch.ID).Msg("No longer satisfied")

						dbConnMutex.Lock()
						defer dbConnMutex.Unlock()

						if err := txSelf.actionRepository.DeleteSatisfaction(action.ID, name); err != nil {
							return errors.WithMessagef(err, "Could not delete satisfaction of input %q on action %q", name, action.ID)
						}

						delete(inputs, name)
					}
				}
			}

			// Fill the CUE expression with the fact's value
			// so it can be referenced by its dependents.
			if input, exists := inputs[name]; exists {
				if err := t.Fill(struct {
					Match interface{} `json:"match"`
				}{input.Value}); err != nil {
					return errors.WithMessagef(err, "Could not fill value input %q with fact %q", name, input.ID)
				}
			}

			return nil
		}); err != nil {
			return errors.WithMessage(err, "While building flow")
		} else {
			return errors.WithMessage(flow.Run(context.Background()), "While running flow")
		}
	})
}
