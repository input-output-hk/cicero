package main

import (
	"context"
	"time"

	"cirello.io/oversight"
)

type AllCmd struct {
}

func runAll(s *AllCmd) error {
	supervisor := oversight.New(oversight.WithSpecification(
		100,                   // number of restarts
		1*time.Minute,         // within this time period
		oversight.OneForOne(), // restart every task on its own
	))

	supervisor.Add((&InvokerCmd{}).start)
	supervisor.Add((&BrainCmd{}).start)
	supervisor.Add((&WebCmd{}).start)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	return supervisor.Start(ctx)
}
