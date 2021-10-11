package main

import (
	"fmt"
	"log"
	"os"

	"github.com/alexflint/go-arg"
)

var buildVersion = "dev"
var buildCommit = "dirty"

var logger = log.New(os.Stderr, "DEBUG: ", log.LstdFlags)

type cicero struct {
	Debug   bool        `arg:"--debug" help:"debugging output"`
	Brain   *BrainCmd   `arg:"subcommand:brain"`
	Invoker *InvokerCmd `arg:"subcommand:invoker"`
	Show    *ShowCmd    `arg:"subcommand:show"`
}

func Version() string {
	return fmt.Sprintf("%s (%s)", buildVersion, buildCommit)
}

func (cicero) Version() string {
	return fmt.Sprintf("cicero %s", Version())
}

func main() {
	args := &cicero{}
	parser, err := parseArgs(args)
	abort(parser, err)

	if args.Debug {
		logger.SetOutput(os.Stderr)
	}

	abort(parser, run(parser, args))
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
	switch {
	case args.Brain != nil:
		return runBrain(args.Brain)
	case args.Invoker != nil:
		return runInvoker(args.Invoker)
	case args.Show != nil:
		return runShow(args.Show)
	default:
		parser.WriteHelp(os.Stderr)
	}

	return nil
}
