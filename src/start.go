package cicero

import (
	"context"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"github.com/vivek-ng/concurrency-limiter/priority"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/application/component"
	"github.com/input-output-hk/cicero/src/application/component/web"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
)

type StartCmd struct {
	Components []string `arg:"positional" help:"any of: create, invoke, fact, nomad, web"`

	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluators     []string `arg:"--evaluators"`
	Env            []string `arg:"--env"`

	WebListen string `arg:"--web-listen" default:":8080"`
}

func (cmd *StartCmd) Run(logger *zerolog.Logger) error {
	logger.Info().Msg("Start...")

	// If none are given then start all,
	// otherwise start only those that are given.
	var start struct {
		actionCreate bool
		actionInvoke bool
		factCreate   bool
		nomadEvent   bool
		web          bool
	}
	for _, component := range cmd.Components {
		switch component {
		case "start":
			start.actionCreate = true
		case "actionInvoke":
			start.actionInvoke = true
		case "factCreate":
			start.factCreate = true
		case "nomad":
			start.nomadEvent = true
		case "web":
			start.web = true
		default:
			logger.Fatal().Msgf("Unknown component: %s", component)
		}
	}
	if !(start.actionCreate ||
		start.actionInvoke ||
		start.factCreate ||
		start.nomadEvent ||
		start.web) {
		start.actionCreate = true
		start.actionInvoke = true
		start.factCreate = true
		start.nomadEvent = true
		start.web = true
	}

	// default to all evaluators we ship
	if len(cmd.Evaluators) == 0 {
		cmd.Evaluators = []string{"nix"}
	}

	db := once(func() interface{} {
		if db, err := config.DBConnection(); err != nil {
			logger.Fatal().Err(err)
			return err
		} else {
			return db
		}
	})

	nomadClient := once(func() interface{} {
		if client, err := config.NewNomadClient(); err != nil {
			logger.Fatal().Err(err)
			return err
		} else {
			return client
		}
	})
	nomadClientWrapper := once(func() interface{} {
		return application.NewNomadClient(nomadClient().(*nomad.Client))
	})

	evaluationService := once(func() interface{} {
		return service.NewEvaluationService(cmd.Evaluators, cmd.Env, logger)
	})
	messageQueueService := once(func() interface{} {
		if bridge, err := config.LiftbridgeConnect(cmd.LiftbridgeAddr); err != nil {
			logger.Fatal().Err(err)
			return err
		} else {
			return service.NewMessageQueueService(db().(*pgxpool.Pool), bridge, logger)
		}
	})
	factService := once(func() interface{} {
		return service.NewFactService(db().(*pgxpool.Pool), logger)
	})
	actionService := once(func() interface{} {
		return service.NewActionService(db().(*pgxpool.Pool), logger)
	})
	runService := once(func() interface{} {
		return service.NewRunService(db().(*pgxpool.Pool), cmd.PrometheusAddr, logger)
	})
	nomadEventService := once(func() interface{} {
		return service.NewNomadEventService(db().(*pgxpool.Pool), runService().(service.RunService), logger)
	})

	supervisor := cmd.newSupervisor(logger)

	if start.actionCreate {
		child := component.ActionCreateConsumer{
			Logger:              logger.With().Str("component", "ActionCreateConsumer").Logger(),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			ActionService:       actionService().(service.ActionService),
			EvaluationService:   evaluationService().(service.EvaluationService),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.actionInvoke {
		child := component.ActionInvokeConsumer{
			EvaluationService:   evaluationService().(service.EvaluationService),
			RunService:          runService().(service.RunService),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			ActionService:       actionService().(service.ActionService),
			Db:                  db().(*pgxpool.Pool),
			NomadClient:         nomadClientWrapper().(application.NomadClient),
			// Increase priority of waiting goroutines every second.
			Limiter: priority.NewLimiter(1, priority.WithDynamicPriority(1000)),
			Logger:  logger.With().Str("component", "WorkflowInvokeConsumer").Logger(),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.factCreate {
		child := component.FactCreateConsumer{
			Logger:              logger.With().Str("component", "FactCreateConsumer").Logger(),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			FactService:         factService().(service.FactService),
			ActionService:       actionService().(service.ActionService),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.nomadEvent {
		child := component.NomadEventConsumer{
			Logger:              logger.With().Str("component", "NomadEventConsumer").Logger(),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			RunService:          runService().(service.RunService),
			NomadEventService:   nomadEventService().(service.NomadEventService),
			NomadClient:         nomadClientWrapper().(application.NomadClient),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.web {
		child := web.Web{
			Logger:              logger.With().Str("component", "Web").Logger(),
			Listen:              cmd.WebListen,
			RunService:          runService().(service.RunService),
			ActionService:       actionService().(service.ActionService),
			FactService:         factService().(service.FactService),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			NomadEventService:   nomadEventService().(service.NomadEventService),
			EvaluationService:   evaluationService().(service.EvaluationService),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := supervisor.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting supervisor")
	}

	<-ctx.Done()
	return nil
}

func (cmd *StartCmd) newSupervisor(logger *zerolog.Logger) *oversight.Tree {
	return oversight.New(
		oversight.WithLogger(&config.SupervisorLogger{Logger: logger}),
		oversight.WithSpecification(
			10,                    // number of restarts
			1*time.Minute,         // within this time period
			oversight.OneForOne(), // restart every task on its own
		),
	)
}

func once(init func() interface{}) func() interface{} {
	var inst *interface{}
	return func() interface{} {
		if inst == nil {
			val := init()
			inst = &val
		}
		return *inst
	}
}
