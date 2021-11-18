package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

type StartCmd struct {
	Brain   bool `arg:"--brain"`
	Invoker bool `arg:"--invoker"`
	Web     bool `arg:"--web"`

	WebListen      string   `arg:"--web-listen" default:":8080"`
	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (cmd *StartCmd) Run() error {
	logger := log.New(os.Stderr, "start: ", log.LstdFlags)

	// If none are given then start all,
	// otherwise start only those that are given.
	if !(cmd.Brain || cmd.Invoker || cmd.Web) {
		cmd.Brain = true
		cmd.Invoker = true
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

	evaluator := once(func() interface{} {
		return NewEvaluator(cmd.Evaluator, cmd.Env)
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
		return NewWorkflowActionService(evaluator().(*Evaluator), workflowService().(service.WorkflowService))
	})
	nomadEventService := once(func() interface{} {
		return service.NewNomadEventService(db().(*pgxpool.Pool), actionService().(service.ActionService))
	})

	supervisor := cmd.newSupervisor(logger)

	if cmd.Brain {
		brain := Brain{
			workflowService:       workflowService().(service.WorkflowService),
			actionService:         actionService().(service.ActionService),
			evaluator:             evaluator().(*Evaluator),
			workflowActionService: workflowActionService().(WorkflowActionService),
			nomadEventService:     nomadEventService().(service.NomadEventService),
			messageQueueService:   messageQueueService().(service.MessageQueueService),
			db:                    db().(*pgxpool.Pool),
			nomadClient:           nomadClient().(*nomad.Client),
			logger:                log.New(os.Stderr, "brain: ", log.LstdFlags),
		}
		supervisor.Add(brain.listenToFacts)
		supervisor.Add(brain.listenToStart)
		supervisor.Add(brain.listenToNomadEvents)
	}

	if cmd.Web {
		web := Web{
			Listen:              cmd.WebListen,
			workflowService:     workflowService().(service.WorkflowService),
			actionService:       actionService().(service.ActionService),
			messageQueueService: messageQueueService().(service.MessageQueueService),
			nomadEventService:   nomadEventService().(service.NomadEventService),
			evaluator:           evaluator().(*Evaluator),
			logger:              log.New(os.Stderr, "web: ", log.LstdFlags),
		}
		supervisor.Add(web.start)
	}

	if cmd.Invoker {
		invoker := Invoker{
			evaluator:           evaluator().(*Evaluator),
			actionService:       actionService().(service.ActionService),
			messageQueueService: messageQueueService().(service.MessageQueueService),
			workflowService:     workflowService().(service.WorkflowService),
			db:                  db().(*pgxpool.Pool),
			nomadClient:         nomadClient().(*nomad.Client),
			// Increase priority of waiting goroutines every second.
			limiter: priority.NewLimiter(1, priority.WithDynamicPriority(1000)),
			logger:  log.New(os.Stderr, "invoker: ", log.LstdFlags),
		}
		supervisor.Add(invoker.listenToInvoke)
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
