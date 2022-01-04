package main

import (
	"fmt"
	"github.com/input-output-hk/cicero/src/domain"
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

	domain.Build.Version = buildVersion
	domain.Build.Commit = buildCommit

	abort(parser, Run(parser, args))
}

type CLI struct {
	Debug bool             `arg:"--debug" help:"debugging output"`
	Start *cicero.StartCmd `arg:"subcommand:start"`
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

func parseArgs(args *CLI) (parser *arg.Parser, err error) {
	parser, err = arg.NewParser(arg.Config{}, args)
	if err != nil {
		return
	}

	err = parser.Parse(os.Args[1:])
	return
}

func Run(parser *arg.Parser, args *CLI) error {
	switch {
	case args.Start != nil:
		return args.Start.Run()
	default:
		parser.WriteHelp(os.Stderr)
	}
	return nil
}
