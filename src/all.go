package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	service "github.com/input-output-hk/cicero/src/service"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
)

type AllCmd struct {
	Addr           string `arg:"--listen" default:":8080"`
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	Evaluator      string `arg:"--evaluator" default:"cicero-evaluator-nix"`
}

func (cmd *AllCmd) Run() error {
	bridge, err := cmd.liftbridgeConnect()
	if err != nil {
		return err
	}

	supervisor := cmd.newSupervisor()

	evaluator := NewEvaluator(cmd.Evaluator)
	workflowService := &service.WorkflowServiceImpl{}
	workflowService.Init(DB)

	brain := &BrainCmd{
		bridge:          bridge,
		workflowService: workflowService,
		evaluator:       evaluator,
	}
	brain.init()

	web := &WebCmd{
		Addr:            cmd.Addr,
		bridge:          bridge,
		workflowService: workflowService,
		evaluator:       evaluator,
	}
	web.init()

	invoker := &InvokerCmd{
		bridge:    bridge,
		evaluator: evaluator,
	}
	invoker.init()

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

func (cmd *AllCmd) liftbridgeConnect() (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{cmd.LiftbridgeAddr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
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
		))
}
