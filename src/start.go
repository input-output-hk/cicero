package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/component"
	"github.com/input-output-hk/cicero/src/component/web"
	"github.com/input-output-hk/cicero/src/config"
)

type StartCmd struct {
	Components []string `arg:"positional" help:"any of: start, invoke, fact, nomad, web"`

	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluators     []string `arg:"--evaluators"`
	Env            []string `arg:"--env"`

	WebListen string `arg:"--web-listen" default:":8080"`
}

func (cmd *StartCmd) Run() error {
	logger := log.New(os.Stderr, "start: ", log.LstdFlags)

	// If none are given then start all,
	// otherwise start only those that are given.
	var start struct {
		workflowStart  bool
		workflowInvoke bool
		workflowFact   bool
		nomadEvent     bool
		web            bool
	}
	for _, component := range cmd.Components {
		switch component {
		case "start":
			start.workflowStart = true
		case "invoke":
			start.workflowInvoke = true
		case "fact":
			start.workflowFact = true
		case "nomad":
			start.nomadEvent = true
		case "web":
			start.web = true
		default:
			logger.Fatalf("Unknown component: %s", component)
		}
	}
	if !(start.workflowStart ||
		start.workflowInvoke ||
		start.workflowFact ||
		start.nomadEvent ||
		start.web) {
		start.workflowStart = true
		start.workflowInvoke = true
		start.workflowFact = true
		start.nomadEvent = true
		start.web = true
	}

	// default to all evaluators we ship
	if len(cmd.Evaluators) == 0 {
		cmd.Evaluators = []string{"nix"}
	}

	db := once(func() interface{} {
		if db, err := config.DBConnection(); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return db
		}
	})

	nomadClient := once(func() interface{} {
		if client, err := config.NewNomadClient(); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return client
		}
	})
	nomadClientWrapper := once(func() interface{} {
		return application.NewNomadClient(nomadClient().(*nomad.Client))
	})

	evaluationService := once(func() interface{} {
		return application.NewEvaluationService(cmd.Evaluators, cmd.Env)
	})
	messageQueueService := once(func() interface{} {
		if bridge, err := config.LiftbridgeConnect(cmd.LiftbridgeAddr); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return application.NewMessageQueueService(db().(*pgxpool.Pool), bridge)
		}
	})
	factService := once(func() interface{} {
		return application.NewFactService(db().(*pgxpool.Pool))
	})
	actionService := once(func() interface{} {
		return application.NewActionService(db().(*pgxpool.Pool))
	})
	runService := once(func() interface{} {
		return application.NewRunService(db().(*pgxpool.Pool), cmd.PrometheusAddr)
	})
	nomadEventService := once(func() interface{} {
		return application.NewNomadEventService(db().(*pgxpool.Pool), runService().(application.RunService))
	})

	supervisor := cmd.newSupervisor(logger)

	if start.workflowStart {
		child := component.ActionStartConsumer{
			Logger:              log.New(os.Stderr, "WorkflowStartConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			ActionService:       actionService().(application.ActionService),
			EvaluationService:   evaluationService().(application.EvaluationService),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.workflowInvoke {
		child := component.InvokeConsumer{
			EvaluationService:   evaluationService().(application.EvaluationService),
			RunService:          runService().(application.RunService),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			ActionService:       actionService().(application.ActionService),
			Db:                  db().(*pgxpool.Pool),
			NomadClient:         nomadClientWrapper().(application.NomadClient),
			// Increase priority of waiting goroutines every second.
			Limiter: priority.NewLimiter(1, priority.WithDynamicPriority(1000)),
			Logger:  log.New(os.Stderr, "invoker: ", log.LstdFlags),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.workflowFact {
		child := component.WorkflowFactConsumer{
			Logger:              log.New(os.Stderr, "WorkflowFactConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			FactService:         factService().(application.FactService),
			ActionService:       actionService().(application.ActionService),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.nomadEvent {
		child := component.NomadEventConsumer{
			Logger:              log.New(os.Stderr, "NomadEventConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			RunService:          runService().(application.RunService),
			NomadEventService:   nomadEventService().(application.NomadEventService),
			NomadClient:         nomadClientWrapper().(application.NomadClient),
			Db:                  db().(*pgxpool.Pool),
		}
		if err := supervisor.Add(child.Start); err != nil {
			return err
		}
	}

	if start.web {
		child := web.Web{
			Listen:              cmd.WebListen,
			Logger:              log.New(os.Stderr, "Web: ", log.LstdFlags),
			RunService:          runService().(application.RunService),
			ActionService:       actionService().(application.ActionService),
			FactService:         factService().(application.FactService),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			NomadEventService:   nomadEventService().(application.NomadEventService),
			EvaluationService:   evaluationService().(application.EvaluationService),
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

func (cmd *StartCmd) newSupervisor(logger *log.Logger) *oversight.Tree {
	return oversight.New(
		oversight.WithLogger(logger),
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
