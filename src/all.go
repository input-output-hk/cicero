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
	Listen         string `arg:"--listen" default:":8080"`
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	Evaluator      string `arg:"--evaluator" default:"cicero-evaluator-nix"`
}

func (cmd *AllCmd) Run() error {
	bridge, err := service.LiftbridgeConnect(cmd.LiftbridgeAddr)
	if err != nil {
		return err
	}

	supervisor := cmd.newSupervisor()

	evaluator := NewEvaluator(cmd.Evaluator)
	workflowService := service.NewWorkflowService(DB, bridge)
	actionService := service.NewActionService(DB)

	brain := Brain{
		workflowService: &workflowService,
		actionService:   &actionService,
		bridge:          &bridge,
		evaluator:       &evaluator,
	}
	(&BrainCmd{}).init(&brain)

	web := Web{
		Listen:          &cmd.Listen,
		bridge:          &bridge,
		workflowService: &workflowService,
		actionService:   &actionService,
		evaluator:       &evaluator,
	}
	(&WebCmd{}).init(&web)

	invoker := Invoker{
		bridge:        &bridge,
		evaluator:     &evaluator,
		actionService: &actionService,
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
