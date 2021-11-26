package cicero

import (
	"context"
	"github.com/input-output-hk/cicero/src/config"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/component"
	"github.com/input-output-hk/cicero/src/component/web"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
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
	workflowService := once(func() interface{} {
		return application.NewWorkflowService(db().(*pgxpool.Pool), messageQueueService().(application.MessageQueueService))
	})
	actionService := once(func() interface{} {
		return application.NewActionService(db().(*pgxpool.Pool), cmd.PrometheusAddr)
	})
	workflowActionService := once(func() interface{} {
		return application.NewWorkflowActionService(evaluationService().(application.EvaluationService), workflowService().(application.WorkflowService))
	})
	nomadEventService := once(func() interface{} {
		return application.NewNomadEventService(db().(*pgxpool.Pool), actionService().(application.ActionService))
	})

	supervisor := cmd.newSupervisor(logger)

	if start.workflowStart {
		child := component.WorkflowStartConsumer{
			Logger:              log.New(os.Stderr, "WorkflowStartConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			WorkflowService:     workflowService().(application.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
		}
		supervisor.Add(child.Start)
	}

	if start.workflowInvoke {
		child := component.WorkflowInvokeConsumer{
			EvaluationService:   evaluationService().(application.EvaluationService),
			ActionService:       actionService().(application.ActionService),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			WorkflowService:     workflowService().(application.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
			NomadClient:         nomadClient().(*nomad.Client),
			// Increase priority of waiting goroutines every second.
			Limiter: priority.NewLimiter(1, priority.WithDynamicPriority(1000)),
			Logger:  log.New(os.Stderr, "invoker: ", log.LstdFlags),
		}
		supervisor.Add(child.Start)
	}

	if start.workflowFact {
		child := component.WorkflowFactConsumer{
			Logger:              log.New(os.Stderr, "WorkflowFactConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			WorkflowService:     workflowService().(application.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
		}
		supervisor.Add(child.Start)
	}

	if start.nomadEvent {
		child := component.NomadEventConsumer{
			Logger:                log.New(os.Stderr, "NomadEventConsumer: ", log.LstdFlags),
			MessageQueueService:   messageQueueService().(application.MessageQueueService),
			WorkflowService:       workflowService().(application.WorkflowService),
			ActionService:         actionService().(application.ActionService),
			WorkflowActionService: workflowActionService().(application.WorkflowActionService),
			NomadEventService:     nomadEventService().(application.NomadEventService),
			NomadClient:           nomadClient().(*nomad.Client),
			Db:                    db().(*pgxpool.Pool),
		}
		supervisor.Add(child.Start)
	}

	if start.web {
		child := web.Web{
			Logger:              log.New(os.Stderr, "Web: ", log.LstdFlags),
			Listen:              cmd.WebListen,
			WorkflowService:     workflowService().(application.WorkflowService),
			ActionService:       actionService().(application.ActionService),
			MessageQueueService: messageQueueService().(application.MessageQueueService),
			NomadEventService:   nomadEventService().(application.NomadEventService),
			EvaluationService:   evaluationService().(application.EvaluationService),
		}
		supervisor.Add(child.Start)
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
