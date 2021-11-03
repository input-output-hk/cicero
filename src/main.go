package cicero

import (
	"context"
	"encoding/json"
	"log"
	"os"
	"time"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgtype"
	pgtypeuuid "github.com/jackc/pgtype/ext/gofrs-uuid"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
)

var DB *pgxpool.Pool
var nomadClient *nomad.Client

func Init() error {
	var err error
	DB, err = openDB(os.Getenv("DATABASE_URL"))
	if err != nil {
		return err
	}

	nomadClient, err = nomad.NewClient(nomad.DefaultConfig())
	if err != nil {
		return err
	}

	return nil
}

func openDB(url string) (*pgxpool.Pool, error) {
	if url == "" {
		return nil, errors.New("The DATABASE_URL environment variable is not set or empty")
	}

	dbconfig, err := pgxpool.ParseConfig(url)
	if err != nil {
		return nil, err
	}

	dbconfig.AfterConnect = func(ctx context.Context, conn *pgx.Conn) error {
		conn.ConnInfo().RegisterDataType(pgtype.DataType{
			Value: &pgtypeuuid.UUID{},
			Name:  "uuid",
			OID:   pgtype.UUIDOID,
		})
		return nil
	}

	return pgxpool.ConnectConfig(context.Background(), dbconfig)
}

func createStreams(logger *log.Logger, bridge liftbridge.Client, streamNames []string) error {
	for _, streamName := range streamNames {
		if err := bridge.CreateStream(
			context.Background(),
			streamName, streamName,
			liftbridge.MaxReplication()); err != nil {
			if err != liftbridge.ErrStreamExists {
				if err != nil {
					time.Sleep(1 * time.Second)
					return errors.WithMessage(err, "Failed to Create NATS Stream")
				}
			}
		} else {
			logger.Printf("Created streams %s\n", streamName)
		}
	}

	return nil
}

func publish(logger *log.Logger, bridge liftbridge.Client, stream, key string, msg WorkflowCerts) error {
	err := createStreams(logger, bridge, []string{stream})
	if err != nil {
		return errors.WithMessage(err, "Before publishing message")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	enc, err := json.Marshal(msg)
	if err != nil {
		return errors.WithMessage(err, "Failed to encode JSON")
	}

	_, err = bridge.Publish(ctx, stream,
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
