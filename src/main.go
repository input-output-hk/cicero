package cicero

import (
	"context"
	"database/sql"
	"encoding/json"
	"log"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
	"github.com/uptrace/bun/dialect/sqlitedialect"
	"github.com/uptrace/bun/driver/sqliteshim"
	nomad "github.com/hashicorp/nomad/api"
)

var DB *bun.DB
var nomadClient *nomad.Client

func Init() error {
	openedDB, err := openDb()
	if err != nil { return err }
	DB = openedDB

	openedNomadClient, err := nomad.NewClient(nomad.DefaultConfig())
	if err != nil { return err }
	nomadClient = openedNomadClient

	return nil
}

func openDb() (*bun.DB, error) {
	sqldb, err := sql.Open(sqliteshim.ShimName, "db/database.sqlite3")
	if err != nil {
		return nil, errors.WithMessage(err, "While opening the DB")
	}

	db := bun.NewDB(sqldb, sqlitedialect.New())

	return db, nil
}

func connect(logger *log.Logger, streamNames []string) (liftbridge.Client, error) {
	client, err := liftbridge.Connect([]string{"127.0.0.1:9292"})
	if err != nil {
		return nil, errors.WithMessage(err, "Couldn't connect to NATS")
	}

	for _, streamName := range streamNames {
		if err := client.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication()); err != nil {
			if err != liftbridge.ErrStreamExists {
				if err != nil {
					time.Sleep(1 * time.Second)
					return nil, errors.WithMessage(err, "Failed to Create NATS Stream")
				}
			}
		} else {
			logger.Printf("Created streams %s\n", streamName)
		}
	}

	return client, nil
}

func publish(logger *log.Logger, stream, key string, msg map[string]interface{}) error {
	client, err := connect(logger, []string{stream})
	if err != nil {
		return err
	}
	defer client.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	enc, err := json.Marshal(msg)
	if err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	}

	_, err = client.Publish(ctx, stream,
		enc,
		liftbridge.Key([]byte(key)),
		liftbridge.PartitionByKey(),
		liftbridge.AckPolicyAll(),
	)

	if err != nil {
		return errors.WithMessage(err, "While publishing message")
	}

	logger.Printf("Published message to stream %s\n", stream)

	return nil
}
