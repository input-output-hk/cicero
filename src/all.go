package cicero

import (
	"context"
	"log"
	"os"
	"time"

	"cirello.io/oversight"
	"github.com/pkg/errors"
)

type AllCmd struct {
	Addr   string `arg:"--listen" default:":8080"`
}

func runAll(s *AllCmd) error {
	supervisor := oversight.New(
		oversight.WithLogger(
			log.New(os.Stderr, "tree: ", log.LstdFlags),
		),
		oversight.WithSpecification(
			5,                     // number of restarts
			1*time.Minute,         // within this time period
			oversight.OneForOne(), // restart every task on its own
		))

	brain := &BrainCmd{}
	brain.init()

	supervisor.Add((&InvokerCmd{}).start)
	supervisor.Add(brain.listenToCerts)
	supervisor.Add(brain.listenToStart)
	supervisor.Add((&WebCmd{
		Addr: s.Addr,
	}).start)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := supervisor.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting supervisor")
	}

	for {
		time.Sleep(time.Hour)
	}

	return nil
}
