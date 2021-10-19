package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/uptrace/bun"
	"github.com/uptrace/bun/dialect/sqlitedialect"
	"github.com/uptrace/bun/driver/sqliteshim"
)

var buildVersion = "dev"
var buildCommit = "dirty"

var db *bun.DB

type cicero struct {
	Debug   bool        `arg:"--debug" help:"debugging output"`
	All     *AllCmd     `arg:"subcommand:all"`
	Brain   *BrainCmd   `arg:"subcommand:brain"`
	Invoker *InvokerCmd `arg:"subcommand:invoker"`
	Web     *WebCmd     `arg:"subcommand:web"`
	Show    *ShowCmd    `arg:"subcommand:show"`
}

func Version() string {
	return fmt.Sprintf("%s (%s)", buildVersion, buildCommit)
}

func (cicero) Version() string {
	return fmt.Sprintf("cicero %s", Version())
}

func main() {
	logger := log.New(os.Stderr, "main: ", log.LstdFlags)
	args := &cicero{}
	parser, err := parseArgs(args)
	abort(parser, err)

	if args.Debug {
		logger.SetOutput(os.Stderr)
	}

	abort(parser, run(parser, args))
}

func openDb() (*bun.DB, error) {
	sqldb, err := sql.Open(sqliteshim.ShimName, "db/database.sqlite3")
	if err != nil {
		return nil, errors.WithMessage(err, "While opening the DB")
	}

	db := bun.NewDB(sqldb, sqlitedialect.New())

	return db, nil
}

func abort(parser *arg.Parser, err error) {
	switch err {
	case nil:
		return
	case arg.ErrHelp:
		parser.WriteHelp(os.Stderr)
		os.Exit(0)
	case arg.ErrVersion:
		fmt.Fprintln(os.Stdout, Version())
		os.Exit(0)
	default:
		fmt.Fprint(os.Stderr, err, "\n")
		os.Exit(1)
	}
}

func parseArgs(args *cicero) (*arg.Parser, error) {
	parser, err := arg.NewParser(arg.Config{}, args)
	if err != nil {
		return nil, err
	}

	return parser, parser.Parse(os.Args[1:])
}

func run(parser *arg.Parser, args *cicero) error {
	openendDb, err := openDb()
	if err != nil {
		return err
	}
	db = openendDb
	defer db.Close()

	switch {
	case args.Brain != nil:
		return args.Brain.run()
	case args.Invoker != nil:
		return args.Invoker.run()
	case args.Web != nil:
		return args.Web.run()
	case args.Show != nil:
		return runShow(args.Show)
	case args.All != nil:
		return runAll(args.All)
	default:
		parser.WriteHelp(os.Stderr)
	}

	return nil
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

func fail(err error) {
	if err != nil {
		panic(err)
	}
}

func publish(logger *log.Logger, stream string, key string, msg map[string]interface{}) error {
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
