package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	service "github.com/input-output-hk/cicero/src/service"
)

type AllCmd struct {
	Addr           string `arg:"--listen" default:":8080"`
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
}

func (cmd *AllCmd) Run() error {
	bridge, err := cmd.liftbridgeConnect()
	if err != nil {
		return err
	}

	supervisor := cmd.newSupervisor()

	workflowService := &service.WorkflowServiceCmd{}
	workflowService.Init(DB)

	brain := &BrainCmd{bridge: bridge, workflowService: workflowService}
	brain.init()

	web := &WebCmd{Addr: cmd.Addr, bridge: bridge, workflowService: workflowService}
	web.init()

	invoker := &InvokerCmd{bridge: bridge}
	invoker.init()

	supervisor.Add(invoker.start)
	supervisor.Add(brain.listenToCerts)
	supervisor.Add(brain.listenToStart)
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
			5,                     // number of restarts
			1*time.Minute,         // within this time period
			oversight.OneForOne(), // restart every task on its own
		))
}
