package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	"github.com/input-output-hk/cicero/src/service"
	"github.com/pkg/errors"
)

type AllCmd struct {
	Listen         string   `arg:"--listen" default:":8080"`
	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (cmd *AllCmd) Run() error {
	bridge, err := service.LiftbridgeConnect(cmd.LiftbridgeAddr)
	if err != nil {
		return err
	}

	supervisor := cmd.newSupervisor()

	evaluator := NewEvaluator(cmd.Evaluator, cmd.Env)
	messageQueueService := service.NewMessageQueueService(DB, bridge)
	workflowService := service.NewWorkflowService(DB, messageQueueService)
	actionService := service.NewActionService(DB, cmd.PrometheusAddr)
	workflowActionService := NewWorkflowActionService(evaluator, workflowService)

	brain := Brain{
		workflowService:       workflowService,
		actionService:         actionService,
		evaluator:             &evaluator,
		workflowActionService: workflowActionService,
		messageQueueService:   messageQueueService,
	}
	(&BrainCmd{}).init(&brain)

	web := Web{
		Listen:              &cmd.Listen,
		workflowService:     workflowService,
		actionService:       actionService,
		messageQueueService: messageQueueService,
		evaluator:           &evaluator,
	}
	(&WebCmd{}).init(&web)

	invoker := Invoker{
		evaluator:           &evaluator,
		actionService:       actionService,
		messageQueueService: messageQueueService,
		workflowService:     workflowService,
	}
	(&InvokerCmd{}).init(&invoker)

	supervisor.Add(invoker.start)
	brain.addToTree(supervisor)
	supervisor.Add(web.start)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := supervisor.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting supervisor")
	}

	for {
		time.Sleep(time.Hour)
	}
}

func (cmd *AllCmd) newSupervisor() *oversight.Tree {
	return oversight.New(
		oversight.WithLogger(
			log.New(os.Stderr, "all: ", log.LstdFlags),
		),
		oversight.WithSpecification(
			10,                    // number of restarts
			1*time.Minute,         // within this time period
			oversight.OneForOne(), // restart every task on its own
		),
	)
}
