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
		10,                    // number of restarts
		10*time.Minute,        // within this time period
		oversight.OneForOne(), // restart every task on its own
	))

	supervisor.Add(invoker)
	supervisor.Add(brain)
	supervisor.Add(web)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	err := invokerSupervisor.Start(ctx)

	return err
}
