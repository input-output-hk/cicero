package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/consumers"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

type StartCmd struct {
	ConsumeWorkflowStart  bool `arg:"--consume-workflow-start"`
	ConsumeWorkflowInvoke bool `arg:"--consume-workflow-invoke"`
	ConsumeWorkflowFact   bool `arg:"--consume-workflow-fact"`
	ConsumeNomadEvent     bool `arg:"--consume-nomad-event"`

	Web       bool   `arg:"--web"`
	WebListen string `arg:"--web-listen" default:":8080"`

	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (cmd *StartCmd) Run() error {
	logger := log.New(os.Stderr, "start: ", log.LstdFlags)

	// If none are given then start all,
	// otherwise start only those that are given.
	if !(cmd.ConsumeWorkflowStart ||
		cmd.ConsumeWorkflowInvoke ||
		cmd.ConsumeWorkflowFact ||
		cmd.ConsumeNomadEvent ||
		cmd.Web) {
		cmd.ConsumeWorkflowStart = true
		cmd.ConsumeWorkflowInvoke = true
		cmd.ConsumeWorkflowFact = true
		cmd.ConsumeNomadEvent = true
		cmd.Web = true
	}

	db := once(func() interface{} {
		if db, err := NewDb(); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return db
		}
	})

	nomadClient := once(func() interface{} {
		if client, err := NewNomadClient(); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return client
		}
	})

	evaluationService := once(func() interface{} {
		return service.NewEvaluationService(cmd.Evaluator, cmd.Env)
	})
	messageQueueService := once(func() interface{} {
		if bridge, err := service.LiftbridgeConnect(cmd.LiftbridgeAddr); err != nil {
			logger.Fatalln(err.Error())
			return err
		} else {
			return service.NewMessageQueueService(db().(*pgxpool.Pool), bridge)
		}
	})
	workflowService := once(func() interface{} {
		return service.NewWorkflowService(db().(*pgxpool.Pool), messageQueueService().(service.MessageQueueService))
	})
	actionService := once(func() interface{} {
		return service.NewActionService(db().(*pgxpool.Pool), cmd.PrometheusAddr)
	})
	workflowActionService := once(func() interface{} {
		return service.NewWorkflowActionService(evaluationService().(service.EvaluationService), workflowService().(service.WorkflowService))
	})
	nomadEventService := once(func() interface{} {
		return service.NewNomadEventService(db().(*pgxpool.Pool), actionService().(service.ActionService))
	})

	supervisor := cmd.newSupervisor(logger)

	if cmd.ConsumeWorkflowStart {
		consumer := consumers.WorkflowStartConsumer{
			Logger:              log.New(os.Stderr, "WorkflowStartConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			WorkflowService:     workflowService().(service.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
		}
		supervisor.Add(consumer.Listen)
	}

	if cmd.ConsumeWorkflowInvoke {
		consumer := consumers.WorkflowInvokeConsumer{
			EvaluationService:   evaluationService().(service.EvaluationService),
			ActionService:       actionService().(service.ActionService),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			WorkflowService:     workflowService().(service.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
			NomadClient:         nomadClient().(*nomad.Client),
			// Increase priority of waiting goroutines every second.
			Limiter: priority.NewLimiter(1, priority.WithDynamicPriority(1000)),
			Logger:  log.New(os.Stderr, "invoker: ", log.LstdFlags),
		}
		supervisor.Add(consumer.Listen)
	}

	if cmd.ConsumeWorkflowFact {
		consumer := consumers.WorkflowFactConsumer{
			Logger:              log.New(os.Stderr, "WorkflowFactConsumer: ", log.LstdFlags),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			WorkflowService:     workflowService().(service.WorkflowService),
			Db:                  db().(*pgxpool.Pool),
		}
		supervisor.Add(consumer.Listen)
	}

	if cmd.ConsumeNomadEvent {
		consumer := consumers.NomadEventConsumer{
			Logger:                log.New(os.Stderr, "NomadEventConsumer: ", log.LstdFlags),
			MessageQueueService:   messageQueueService().(service.MessageQueueService),
			WorkflowService:       workflowService().(service.WorkflowService),
			ActionService:         actionService().(service.ActionService),
			WorkflowActionService: workflowActionService().(service.WorkflowActionService),
			NomadEventService:     nomadEventService().(service.NomadEventService),
			NomadClient:           nomadClient().(*nomad.Client),
			Db:                    db().(*pgxpool.Pool),
		}
		supervisor.Add(consumer.Listen)
	}

	if cmd.Web {
		web := Web{
			Logger:              log.New(os.Stderr, "Web: ", log.LstdFlags),
			Listen:              cmd.WebListen,
			WorkflowService:     workflowService().(service.WorkflowService),
			ActionService:       actionService().(service.ActionService),
			MessageQueueService: messageQueueService().(service.MessageQueueService),
			NomadEventService:   nomadEventService().(service.NomadEventService),
			EvaluationService:   evaluationService().(service.EvaluationService),
		}
		supervisor.Add(web.Start)
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
