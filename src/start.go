package cicero

import (
	"context"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/application/component"
	"github.com/input-output-hk/cicero/src/application/component/web"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
)

//go:generate mockery --all --keeptree

type StartCmd struct {
	Components []string `arg:"positional" help:"any of: nomad, web"`

	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluators     []string `arg:"--evaluators"`
	Env            []string `arg:"--env"`

	WebListen string `arg:"--web-listen,env:WEB_LISTEN" default:":8080"`
}

func (cmd *StartCmd) Run(logger *zerolog.Logger) error {
	logger.Info().Msg("Starting components")

	// If none are given then start all,
	// otherwise start only those that are given.
	var start struct {
		factCreate bool
		nomadEvent bool
		web        bool
	}
	for _, component := range cmd.Components {
		switch component {
		case "nomad":
			start.nomadEvent = true
		case "web":
			start.web = true
		default:
			logger.Fatal().Msgf("Unknown component: %s", component)
		}
	}
	if !(start.factCreate ||
		start.nomadEvent ||
		start.web) {
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
			logger.Fatal().Err(err).Send()
			return nil
		} else {
			return db
		}
	})

	nomadClient := once(func() interface{} {
		if client, err := config.NewNomadClient(); err != nil {
			logger.Fatal().Err(err).Send()
			return nil
		} else {
			return client
		}
	})
	nomadClientWrapper := once(func() interface{} {
		return application.NewNomadClient(nomadClient().(*nomad.Client))
	})

	runService := once(func() interface{} {
		return service.NewRunService(db().(config.PgxIface), cmd.PrometheusAddr, nomadClientWrapper().(application.NomadClient), logger)
	})
	evaluationService := once(func() interface{} {
		return service.NewEvaluationService(cmd.Evaluators, cmd.Env, logger)
	})
	actionService := once(func() interface{} {
		return service.NewActionService(db().(config.PgxIface), nomadClientWrapper().(application.NomadClient), runService().(service.RunService), evaluationService().(service.EvaluationService), logger)
	})
	factService := once(func() interface{} {
		return service.NewFactService(db().(config.PgxIface), actionService().(service.ActionService), logger)
	})
	nomadEventService := once(func() interface{} {
		return service.NewNomadEventService(db().(config.PgxIface), runService().(service.RunService), logger)
	})

	supervisor := cmd.newSupervisor(logger)

	if start.nomadEvent {
		child := component.NomadEventConsumer{
			Logger:            logger.With().Str("component", "NomadEventConsumer").Logger(),
			RunService:        runService().(service.RunService),
			NomadEventService: nomadEventService().(service.NomadEventService),
			FactService:       factService().(service.FactService),
			NomadClient:       nomadClientWrapper().(application.NomadClient),
			Db:                db().(config.PgxIface),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.web {
		child := web.Web{
			Logger:            logger.With().Str("component", "Web").Logger(),
			Listen:            cmd.WebListen,
			RunService:        runService().(service.RunService),
			ActionService:     actionService().(service.ActionService),
			FactService:       factService().(service.FactService),
			NomadEventService: nomadEventService().(service.NomadEventService),
			EvaluationService: evaluationService().(service.EvaluationService),
			Db:                db().(config.PgxIface),
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
