package main

import (
	"flag"
	"fmt"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/rs/zerolog"
	"os"

	"github.com/alexflint/go-arg"
	cicero "github.com/input-output-hk/cicero/src"
)

var buildVersion = "dev"
var buildCommit = "dirty"

func main() {
	args := &CLI{}
	parser, err := parseArgs(args)
	abort(parser, err)

	debug := flag.Bool("debug", args.Debug, "sets log level to debug")
	logger := config.ConfigureLogger(*debug)

	domain.BuildInfo.Version = buildVersion
	domain.BuildInfo.Commit = buildCommit

	abort(parser, Run(parser, args, logger))
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

func Run(parser *arg.Parser, args *CLI, logger *zerolog.Logger) error {
	switch {
	case args.Start != nil:
		return args.Start.Run(logger)
	default:
		parser.WriteHelp(os.Stderr)
	}
	return nil
}
