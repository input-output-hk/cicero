package main

import (
	"fmt"
	"os"

	"github.com/alexflint/go-arg"
	"github.com/rs/zerolog"

	cicero "github.com/input-output-hk/cicero/src"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
)

var buildVersion = "dev"
var buildCommit = "dirty"

func main() {
	args := &CLI{}
	parser, err := parseArgs(args)
	abort(parser, err)

	level, err := zerolog.ParseLevel(args.LogLevel)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	logger := config.ConfigureLogger(level)

	domain.Build.Version = buildVersion
	domain.Build.Commit = buildCommit

	abort(parser, Run(parser, args, logger))
}

type CLI struct {
	LogLevel string           `arg:"--log-level" default:"info"`
	Start    *cicero.StartCmd `arg:"subcommand:start"`
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
		fmt.Fprintln(os.Stderr, err)
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
