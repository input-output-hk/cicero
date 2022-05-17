package service

import (
	"context"
	"sync"

	"cuelang.org/go/cue"
	cueliteral "cuelang.org/go/cue/literal"
	"cuelang.org/go/tools/flow"
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
	withQuerier(config.PgxIface, ActionServiceCyclicDependencies) ActionService

	GetById(uuid.UUID) (domain.Action, error)
	GetByInvocationId(uuid.UUID) (domain.Action, error)
	GetByRunId(uuid.UUID) (domain.Action, error)
	GetByName(string, *repository.Page) ([]*domain.Action, error)
	GetLatestByName(string) (domain.Action, error)
	GetAll() ([]*domain.Action, error)
	GetCurrent() ([]*domain.Action, error)
	GetCurrentActive() ([]*domain.Action, error)
	Save(*domain.Action) error
	Update(*domain.Action) error
	IsRunnable(*domain.Action) (bool, map[string]*domain.Fact, error)
	Create(string, string) (*domain.Action, error)
	// Returns a nil pointer for the first return value if the Action was not runnable.
	Invoke(*domain.Action) (*domain.Run, func() error, error)
	InvokeCurrentActive() ([]EvaluationError, error)
}

type ActionServiceCyclicDependencies struct {
	invocationService *InvocationService
}

type actionService struct {
	logger            zerolog.Logger
	actionRepository  repository.ActionRepository
	factRepository    repository.FactRepository
	evaluationService EvaluationService
	runService        RunService
	nomadClient       application.NomadClient
	db                config.PgxIface
	ActionServiceCyclicDependencies
}

func NewActionService(db config.PgxIface, nomadClient application.NomadClient, invocationService *InvocationService, runService RunService, evaluationService EvaluationService, logger *zerolog.Logger) ActionService {
	return &actionService{
		logger:            logger.With().Str("component", "ActionService").Logger(),
		actionRepository:  persistence.NewActionRepository(db),
		factRepository:    persistence.NewFactRepository(db),
		evaluationService: evaluationService,
		nomadClient:       nomadClient,
		runService:        runService,
		db:                db,
		ActionServiceCyclicDependencies: ActionServiceCyclicDependencies{
			invocationService: invocationService,
		},
	}
}

func (self actionService) WithQuerier(querier config.PgxIface) ActionService {
	return self.withQuerier(querier, ActionServiceCyclicDependencies{})
}

func (self actionService) withQuerier(querier config.PgxIface, cyclicDeps ActionServiceCyclicDependencies) ActionService {
	result := actionService{
		logger:            self.logger,
		actionRepository:  self.actionRepository.WithQuerier(querier),
		factRepository:    self.factRepository.WithQuerier(querier),
		runService:        self.runService.WithQuerier(querier),
		evaluationService: self.evaluationService,
		nomadClient:       self.nomadClient,
		db:                querier,
		ActionServiceCyclicDependencies: cyclicDeps,
	}

	if result.invocationService == nil {
		r := ActionService(result)
		result.invocationService = new(InvocationService)
		*result.invocationService = (*self.invocationService).withQuerier(querier, InvocationServiceCyclicDependencies{actionService: &r})
	}

	return &result
}

func (self actionService) GetById(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by ID")
	action, err = self.actionRepository.GetById(id)
	err = errors.WithMessagef(err, "Could not select existing Action for ID %q", id)
	return
}

func (self actionService) GetByRunId(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by Run ID")
	action, err = self.actionRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Run ID %q", id)
	return
}

func (self actionService) GetByInvocationId(id uuid.UUID) (action domain.Action, err error) {
	self.logger.Trace().Str("id", id.String()).Msg("Getting Action by Invocation ID")
	action, err = self.actionRepository.GetByInvocationId(id)
	err = errors.WithMessagef(err, "Could not select existing Action for Invocation ID %q", id)
	return
}

func (self actionService) GetByName(name string, page *repository.Page) (actions []*domain.Action, err error) {
	self.logger.Trace().Str("name", name).Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting Actions by name")
	actions, err = self.actionRepository.GetByName(name, page)
	err = errors.WithMessagef(err, "Could not select Actions for name %q with offset %d and limit %d", name, page.Offset, page.Limit)
	return
}

func (self actionService) GetLatestByName(name string) (action domain.Action, err error) {
	self.logger.Trace().Str("name", name).Msg("Getting latest Action by name")
	action, err = self.actionRepository.GetLatestByName(name)
	err = errors.WithMessagef(err, "Could not select latest Action for name %q", name)
	return
}

func (self actionService) GetAll() ([]*domain.Action, error) {
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

func (self actionService) GetCurrent() (actions []*domain.Action, err error) {
	self.logger.Trace().Msg("Getting current Actions")
	actions, err = self.actionRepository.GetCurrent()
	err = errors.WithMessagef(err, "Could not select current Actions")
	return
}

func (self actionService) GetCurrentActive() (actions []*domain.Action, err error) {
	self.logger.Trace().Msg("Getting current active Actions")
	actions, err = self.actionRepository.GetCurrentActive()
	err = errors.WithMessagef(err, "Could not select current active Actions")
	return
}

func (self actionService) IsRunnable(action *domain.Action) (bool, map[string]*domain.Fact, error) {
	logger := self.logger.With().
		Str("name", action.Name).
		Str("id", action.ID.String()).
		Logger()

	logger.Debug().Msg("Checking whether Action is runnable")

	inputs := map[string]*domain.Fact{}

	{ // Select and match candidate facts.
		dbConnMutex := &sync.Mutex{}
		errNotRunnable := errors.New("not runnable")
		valuePath := cue.MakePath(cue.Str("value"))

		if err := action.InOut.Inputs.Flow(func(t *flow.Task) error {
			name := t.Path().Selectors()[1].String() // inputs: <name>: …
			if name_, err := cueliteral.Unquote(name); err == nil {
				name = name_
			}
			input := action.InOut.Inputs[name]
			tValue := t.Value().LookupPath(valuePath)
			inputLogger := logger.With().Str("input", name).Logger()

			// A transaction happens on exactly one connection so
			// we cannot use more connections to run queries in parallel.
			dbConnMutex.Lock()
			defer dbConnMutex.Unlock()

			switch fact, err := self.factRepository.GetLatestByCue(tValue); {
			case pgxscan.NotFound(err):
				if !input.Not && !input.Optional {
					inputLogger.Debug().
						Bool("runnable", false).
						Msg("No fact found for required input")
					return errNotRunnable
				}
			case err != nil:
				return err
			default:
				if !input.Not {
					inputs[name] = &fact
				}

				// Match candidate fact.
				if matchErr, err := matchFact(input.Match.ValueWithInputs(inputs), &fact); err != nil {
					return err
				} else if (matchErr == nil) == input.Not {
					if !input.Optional || input.Not {
						inputLogger.Debug().
							Bool("runnable", false).
							Str("fact", fact.ID.String()).
							AnErr("mismatch", matchErr).
							Msg("Fact does not match")
						return errNotRunnable
					}
					delete(inputs, name)
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
		}).Run(context.Background()); err != nil {
			if errors.Is(err, errNotRunnable) {
				return false, inputs, nil
			}
			return false, nil, err
		}
	}

	// Not runnable if the inputs are the same as last run.
	if run, err := self.runService.GetLatestByActionId(action.ID); err != nil {
		if !pgxscan.NotFound(err) {
			return false, nil, err
		}
	} else {
		var inputFactIds map[string]uuid.UUID
		if inputFactIds, err = (*self.invocationService).GetInputFactIdsById(run.InvocationId); err != nil {
			if !pgxscan.NotFound(err) {
				return false, nil, err
			}
			inputFactIds = map[string]uuid.UUID{}
		}

		inputFactsChanged := false

	InputFactsChanged:
		for name, input := range action.InOut.Inputs {
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
				if inputs[name] != nil {
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
				if inputs[name] == nil || *oldFactId != inputs[name].ID {
					if logger.Debug().Enabled() {
						var newFactIdToLog *string
						if inputs[name] != nil {
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
				// A previous Run would not have been started
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
				Msg("input satisfied by same Fact(s) as last Run")
		}

		if !inputFactsChanged {
			return false, inputs, nil
		}
	}

	// Filter input facts. We only provide keys requested by the CUE expression.
	for name, input := range action.InOut.Inputs {
		if entry, exists := inputs[name]; exists {
			filterFields(&entry.Value, input.Match.ValueWithInputs(inputs))
		}
	}

	logger.Debug().Bool("runnable", true).Send()
	return true, inputs, nil
}

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

func matchFact(match cue.Value, fact *domain.Fact) (error, error) {
	factCue := match.Context().Encode(fact.Value)
	if err := factCue.Err(); err != nil {
		return nil, err
	}
	return match.Unify(factCue).Validate(cue.Final()), nil
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
		txSelf := self.WithQuerier(tx)

		// deactivate previous version for convenience
		if prev, err := txSelf.GetLatestByName(action.Name); err != nil && !pgxscan.NotFound(err) {
			return err
		} else if err == nil && prev.Active {
			prev.Active = false
			if err := txSelf.Update(&prev); err != nil {
				return err
			}
		}

		return txSelf.Save(&action)
	}); err != nil {
		return nil, err
	}

	return &action, nil
}

func (self actionService) Invoke(action *domain.Action) (*domain.Run, func() error, error) {
	var run *domain.Run
	var registerFunc func() error
	var evalErr *EvaluationError

	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		txSelf := self.WithQuerier(tx)

		runnable, inputs, err := txSelf.IsRunnable(action)
		if err != nil || !runnable {
			return err
		}

		invocation := domain.Invocation{ActionId: action.ID}

		if err := (*self.invocationService).WithQuerier(tx).Save(&invocation, inputs); err != nil {
			return err
		}

		job, err := self.evaluationService.EvaluateRun(action.Source, action.Name, action.ID, inputs)
		if err != nil {
			if errors.As(err, &evalErr) {
				self.logger.Err(evalErr).
					Str("source", action.Source).
					Str("name", action.Name).
					Msg("Could not evaluate action")

				{
					stdout := string(evalErr.Stdout)
					invocation.EvalStdout = &stdout

					stderr := string(evalErr.Stderr)
					invocation.EvalStderr = &stderr
				}
				if err := (*self.invocationService).WithQuerier(tx).Update(&invocation); err != nil {
					return err
				}

				// Do not return the evalErr so that the transaction commits
				// and the invocation is updated with the error.
				return nil
			} else {
				return err
			}
		}

		tmpRun := domain.Run{InvocationId: invocation.Id}

		if err := self.runService.WithQuerier(tx).Save(&tmpRun); err != nil {
			return errors.WithMessage(err, "Could not insert Run")
		}

		if job == nil { // An action that has no job is called a decision.
			if success := action.InOut.Output.WithInputs(inputs).Success(); success != nil {
				fact := domain.Fact{
					RunId: &tmpRun.NomadJobID,
					Value: success,
				}
				if err := self.factRepository.WithQuerier(tx).Save(&fact, nil); err != nil {
					return errors.WithMessage(err, "Could not publish fact")
				}
			}

			tmpRun.CreatedAt = tmpRun.CreatedAt.UTC()
			tmpRun.FinishedAt = &tmpRun.CreatedAt

			err := self.runService.WithQuerier(tx).Update(&tmpRun)
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
		return run, registerFunc, err
	}

	if evalErr != nil {
		return run, registerFunc, evalErr
	}
	return run, registerFunc, nil
}

func (self actionService) InvokeCurrentActive() ([]EvaluationError, error) {
	evalErrs := []EvaluationError{}
	return evalErrs, self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		registerFuncs := []func() error{}

		txSelf := self.WithQuerier(tx)

		actions, err := txSelf.GetCurrentActive()
		if err != nil {
			return err
		}

		for {
			anyRunnable := false

			for _, action := range actions {
				if job, registerFunc, err := txSelf.Invoke(action); err != nil {
					var evalErr *EvaluationError
					if errors.As(err, &evalErr) {
						evalErrs = append(evalErrs, *evalErr)
					} else {
						return err
					}
				} else {
					anyRunnable = anyRunnable || job != nil
					if registerFunc != nil {
						registerFuncs = append(registerFuncs, registerFunc)
					}
				}
			}

			if !anyRunnable {
				break
			}
		}

		for _, registerFunc := range registerFuncs {
			if err := registerFunc(); err != nil {
				return err
			}
		}

		return nil
	})
}
