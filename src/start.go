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

type InstanceOpts interface {
	NewDB(*zerolog.Logger) (*pgxpool.Pool, error)
	NewNomadClient() (*nomad.Client, error)
	NewPrometheusClient() (prometheus.Client, error)
	NewPromtailClient(*zerolog.Logger) (promtailClient.Client, error)
	GetComponentOpts() InstanceComponentsOpts
	GetEvaluators() []string
	GetVictoriaMetricsAddr() string
	GetTransformers() []string
}

type InstanceComponentsOpts struct {
	NomadEvent bool
	Web        *InstanceWebComponentOpts
}

type InstanceWebComponentOpts struct {
	ListenAddr string
	CookieAuth string
	CookieEnc string
	OidcProviders string
}

func (cmd StartCmd) NewDB(logger *zerolog.Logger) (*pgxpool.Pool, error) {
	return config.DBConnection(logger, cmd.LogDb)
}

func (cmd StartCmd) NewNomadClient() (*nomad.Client, error) {
	return config.NewNomadClient()
}

func (cmd StartCmd) NewPrometheusClient() (prometheus.Client, error) {
	return prometheus.NewClient(prometheus.Config{
		Address: cmd.PrometheusAddr,
	})
}

func (cmd StartCmd) NewPromtailClient(logger *zerolog.Logger) (promtailClient.Client, error) {
	return config.NewPromtailClient(cmd.PrometheusAddr, logger)
}

func (cmd StartCmd) GetComponentOpts() InstanceComponentsOpts {
	start := InstanceComponentsOpts{}

	webOpts := InstanceWebComponentOpts{
		ListenAddr: cmd.WebListen,
		CookieAuth: cmd.WebCookieAuth,
		CookieEnc: cmd.WebCookieEnc,
		OidcProviders: cmd.WebOidcProviders,
	}

	// If none are given then start all,
	// otherwise start only those that are given.
	for _, component := range cmd.Components {
		switch component {
		case "nomad":
			start.NomadEvent = true
		case "web":
			start.Web = &webOpts
		default:
			panic("Unknown component: " + component)
		}
	}
	if !start.NomadEvent && start.Web == nil {
		start.NomadEvent = true
		start.Web = &webOpts
	}

	return start
}

func (cmd StartCmd) GetEvaluators() []string {
	// default to all evaluators we ship
	if len(cmd.Evaluators) == 0 {
		cmd.Evaluators = []string{"nix"}
	}
	return cmd.Evaluators
}

func (cmd StartCmd) GetVictoriaMetricsAddr() string {
	return cmd.VictoriaMetricsAddr
}

func (cmd StartCmd) GetTransformers() []string {
	return cmd.Transformers
}

func NewInstance(opts InstanceOpts, logger *zerolog.Logger) (Instance, error) {
	instance := Instance{logger: logger}

	if db, err := opts.NewDB(logger); err != nil {
		logger.Fatal().Err(err).Send()
		return instance, err
	} else {
		instance.db = db
	}

	if client, err := opts.NewNomadClient(); err != nil {
		logger.Fatal().Err(err).Send()
		return instance, err
	} else {
		instance.nomadClient = client
	}
	nomadClientWrapper := application.NewNomadClient(instance.nomadClient)

	var prometheusClient prometheus.Client
	if client, err := opts.NewPrometheusClient(); err != nil {
		logger.Fatal().Err(err).Send()
		return instance, err
	} else {
		prometheusClient = client
	}

	if client, err := opts.NewPromtailClient(logger); err != nil {
		logger.Fatal().Err(err).Send()
		return instance, err
	} else {
		instance.promtailClient = client
	}

	// These are pointers to interfaces to allow them do cyclically depend on each other.
	invocationService := new(service.InvocationService)
	actionService := new(service.ActionService)
	factService := new(service.FactService)

	// These don't cyclically depend on other services so we don't need to put them behind a pointer.
	lokiService := service.NewLokiService(prometheusClient, logger)
	nomadEventService := service.NewNomadEventService(instance.db, logger)
	runService := service.NewRunService(instance.db, lokiService, nomadEventService, opts.GetVictoriaMetricsAddr(), nomadClientWrapper, logger)
	evaluationService := service.NewEvaluationService(opts.GetEvaluators(), opts.GetTransformers(), instance.promtailClient.Chan(), logger)
	actionNameService := service.NewActionNameService(instance.db, logger)
	sessionService := service.NewSessionService(instance.db, logger)

	*invocationService = service.NewInvocationService(instance.db, lokiService, actionService, factService, logger)
	*actionService = service.NewActionService(instance.db, nomadClientWrapper, invocationService, factService, runService, evaluationService, logger)
	*factService = service.NewFactService(instance.db, actionService, logger)

	start := opts.GetComponentOpts()

	if start.NomadEvent {
		instance.Nomad = &component.NomadEventConsumer{
			Logger:            logger.With().Str("component", "NomadEventConsumer").Logger(),
			RunService:        runService,
			NomadEventService: nomadEventService,
			FactService:       *factService,
			InvocationService: *invocationService,
			NomadClient:       nomadClientWrapper,
			Db:                instance.db,
		}
	}

	if start.Web != nil {
		cfg, err := config.NewWebConfig(start.Web.ListenAddr, start.Web.CookieAuth, start.Web.CookieEnc, start.Web.OidcProviders)
		if err != nil {
			return instance, err
		}
		instance.Web = &web.Web{
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
			Db:                instance.db,
		}
	}

	return instance, nil
}

type Instance struct {
	Nomad *component.NomadEventConsumer
	Web *web.Web

	logger *zerolog.Logger
	db *pgxpool.Pool
	promtailClient promtailClient.Client
	nomadClient *nomad.Client
}

func (self Instance) Close() {
	self.db.Close()
	self.nomadClient.Close()
	self.promtailClient.Stop()
}

func (self Instance) Run(ctx context.Context) error {
	self.logger.Info().Msg("Starting components")

	supervisor := oversight.New(
		oversight.WithLogger(&config.SupervisorLogger{Logger: self.logger}),
		oversight.WithSpecification(
			10,                    // number of restarts
			1*time.Minute,         // within this time period
			oversight.OneForOne(), // restart every task on its own
		),
	)

	if self.Nomad != nil {
		if err := supervisor.Add(self.Nomad.Start); err != nil {
			return err
		}
	}

	if self.Web != nil {
		if err := supervisor.Add(self.Web.Start); err != nil {
			return err
		}
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if err := supervisor.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting supervisor")
	}

	<-ctx.Done()
	return nil
}
