package cicero

import (
	"context"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
)

type AllCmd struct {
	Listen         string   `arg:"--listen" default:":8080"`
	LiftbridgeAddr string   `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	PrometheusAddr string   `arg:"--prometheus-addr" default:"http://127.0.0.1:3100"`
	Evaluator      string   `arg:"--evaluator" default:"cicero-evaluator-nix"`
	Env            []string `arg:"--env"`
}

func (cmd *AllCmd) Run(db *pgxpool.Pool, nomadClient *nomad.Client) error {
	bridge, err := application.LiftbridgeConnect(cmd.LiftbridgeAddr)
	if err != nil {
		return err
	}

	supervisor := cmd.newSupervisor()

	evaluator := application.NewEvaluator(cmd.Evaluator, cmd.Env)
	messageQueueService := application.NewMessageQueueService(db, bridge)
	workflowService := application.NewWorkflowService(db, messageQueueService)
	actionService := application.NewActionService(db, cmd.PrometheusAddr)
	workflowActionService := application.NewWorkflowActionService(evaluator, workflowService)
	nomadEventService := application.NewNomadEventService(db, actionService)

	brain := Brain{
		workflowService:       workflowService,
		actionService:         actionService,
		evaluator:             &evaluator,
		workflowActionService: workflowActionService,
		nomadEventService:     nomadEventService,
		messageQueueService:   messageQueueService,
		db:                    db,
		nomadClient:           nomadClient,
	}
	(&BrainCmd{}).init(&brain, db, nomadClient)

	web := Web{
		Listen:              &cmd.Listen,
		workflowService:     workflowService,
		actionService:       actionService,
		messageQueueService: messageQueueService,
		nomadEventService:   nomadEventService,
		evaluator:           &evaluator,
	}
	(&WebCmd{}).init(&web, db)

	invoker := Invoker{
		evaluator:           &evaluator,
		actionService:       actionService,
		messageQueueService: messageQueueService,
		workflowService:     workflowService,
		db:                  db,
		nomadClient:         nomadClient,
	}
	(&InvokerCmd{}).init(&invoker, db, nomadClient)

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
