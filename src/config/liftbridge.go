package config

import (
	"github.com/liftbridge-io/go-liftbridge/v2"
	"github.com/pkg/errors"
)

func LiftbridgeConnect(addr string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{addr})
	return client, errors.WithMessage(err, "Couldn't connect to NATS")
}