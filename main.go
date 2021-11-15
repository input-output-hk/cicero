package main

import (
	"fmt"
	"log"
	"os"

	"github.com/alexflint/go-arg"
	cicero "github.com/input-output-hk/cicero/src"
)

var buildVersion = "dev"
var buildCommit = "dirty"

func main() {
	logger := log.New(os.Stderr, "main: ", log.LstdFlags)
	args := &CLI{}
	parser, err := parseArgs(args)
	abort(parser, err)

	if args.Debug {
		logger.SetOutput(os.Stderr)
	}

	abort(parser, Run(parser, args))
}

type CLI struct {
	Debug   bool               `arg:"--debug" help:"debugging output"`
	All     *cicero.AllCmd     `arg:"subcommand:all"`
	Brain   *cicero.BrainCmd   `arg:"subcommand:brain"`
	Invoker *cicero.InvokerCmd `arg:"subcommand:invoker"`
	Web     *cicero.WebCmd     `arg:"subcommand:web"`
}

func Version() string {
	return fmt.Sprintf("%s (%s)", buildVersion, buildCommit)
}

func (CLI) Version() string {
	return fmt.Sprintf("cicero %s", Version())
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

func parseArgs(args *CLI) (*arg.Parser, error) {
	parser, err := arg.NewParser(arg.Config{}, args)
	if err != nil {
		return nil, err
	}

	err = parser.Parse(os.Args[1:])
	return parser, err
}

func Run(parser *arg.Parser, args *CLI) error {
	db, nomadClient, err := cicero.Init()
	if err != nil {
		return err
	}

	defer db.Close()

	switch {
	case args.Brain != nil:
		return args.Brain.Run(db, nomadClient)
	case args.Invoker != nil:
		return args.Invoker.Run(db, nomadClient)
	case args.Web != nil:
		return args.Web.Run(db)
	case args.All != nil:
		return args.All.Run(db, nomadClient)
	default:
		parser.WriteHelp(os.Stderr)
	}

	return nil
}
