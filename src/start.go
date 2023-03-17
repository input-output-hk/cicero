package cicero

import (
	"context"
	"time"

	"cirello.io/oversight"
	promtailClient "github.com/grafana/loki/clients/pkg/promtail/client"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/application/component"
	"github.com/input-output-hk/cicero/src/application/component/web"
	"github.com/input-output-hk/cicero/src/application/service"
	"github.com/input-output-hk/cicero/src/config"
)

//go:generate mockery --all --keeptree

type StartCmd struct {
	Components []string `arg:"positional,env:CICERO_COMPONENTS" help:"any of: nomad, web"`

	PrometheusAddr      string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	VictoriaMetricsAddr string   `arg:"--victoriametrics-addr" default:"http://127.0.0.1:8428"`
	Evaluators          []string `arg:"--evaluators"`
	Transformers        []string `arg:"--transform"`

	WebListen        string `arg:"--web-listen,env:CICERO_WEB_LISTEN" default:":8080"`
	WebCookieAuth    string `arg:"--web-cookie-auth" help:"file that contains the cookie authentication key"`
	WebCookieEnc     string `arg:"--web-cookie-enc" help:"file that contains the cookie encryption key"`
	WebOidcProviders string `arg:"--web-oidc-providers" help:"JSON file that contains a map of OIDC provider settings"`

	LogDb bool `arg:"--log-db"`
}

func (cmd *StartCmd) Run(logger *zerolog.Logger) error {
	logger.Info().Msg("Starting components")

	// If none are given then start all,
	// otherwise start only those that are given.
	var start struct {
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
	if !(start.nomadEvent ||
		start.web) {
		start.nomadEvent = true
		start.web = true
	}

	// default to all evaluators we ship
	if len(cmd.Evaluators) == 0 {
		cmd.Evaluators = []string{"nix"}
	}

	var db *pgxpool.Pool
	if db_, err := config.DBConnection(logger, cmd.LogDb); err != nil {
		logger.Fatal().Err(err).Send()
		return err
	} else {
		db = db_
	}

	var nomadClient *nomad.Client
	if client, err := config.NewNomadClient(); err != nil {
		logger.Fatal().Err(err).Send()
		return err
	} else {
		nomadClient = client
	}
	nomadClientWrapper := application.NewNomadClient(nomadClient)

	var prometheusClient prometheus.Client
	if client, err := prometheus.NewClient(prometheus.Config{
		Address: cmd.PrometheusAddr,
	}); err != nil {
		logger.Fatal().Err(err).Send()
		return err
	} else {
		prometheusClient = client
	}

	var promtailClient promtailClient.Client
	if client, err := config.NewPromtailClient(cmd.PrometheusAddr, logger); err != nil {
		logger.Fatal().Err(err).Send()
		return err
	} else {
		promtailClient = client
		defer promtailClient.Stop()
	}

	// These are pointers to interfaces to allow them do cyclically depend on each other.
	invocationService := new(service.InvocationService)
	actionService := new(service.ActionService)
	factService := new(service.FactService)

	// These don't cyclically depend on other services so we don't need to put them behind a pointer.
	lokiService := service.NewLokiService(prometheusClient, logger)
	nomadEventService := service.NewNomadEventService(db, logger)
	runService := service.NewRunService(db, lokiService, nomadEventService, cmd.VictoriaMetricsAddr, nomadClientWrapper, logger)
	evaluationService := service.NewEvaluationService(cmd.Evaluators, cmd.Transformers, promtailClient.Chan(), logger)
	actionNameService := service.NewActionNameService(db, logger)
	sessionService := service.NewSessionService(db, logger)

	*invocationService = service.NewInvocationService(db, lokiService, actionService, factService, logger)
	*actionService = service.NewActionService(db, nomadClientWrapper, invocationService, factService, runService, evaluationService, logger)
	*factService = service.NewFactService(db, actionService, logger)

	supervisor := cmd.newSupervisor(logger)

	if start.nomadEvent {
		child := component.NomadEventConsumer{
			Logger:            logger.With().Str("component", "NomadEventConsumer").Logger(),
			RunService:        runService,
			NomadEventService: nomadEventService,
			FactService:       *factService,
			InvocationService: *invocationService,
			NomadClient:       nomadClientWrapper,
			Db:                db,
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.web {
		cfg, err := config.NewWebConfig(cmd.WebListen, cmd.WebCookieAuth, cmd.WebCookieEnc, cmd.WebOidcProviders)
		if err != nil {
			return err
		}
		child := web.Web{
			Config:            cfg,
			Logger:            logger.With().Str("component", "Web").Logger(),
			InvocationService: *invocationService,
			RunService:        runService,
			ActionService:     *actionService,
			ActionNameService: actionNameService,
			FactService:       *factService,
			NomadEventService: nomadEventService,
			EvaluationService: evaluationService,
			SessionService:    sessionService,
			Db:                db,
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
