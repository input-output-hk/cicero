package main

import (
	"fmt"
	"log"
	"os"

	"github.com/alexflint/go-arg"
	cicero "github.com/input-output-hk/cicero/src"
)

func main() {
	logger := log.New(os.Stderr, "main: ", log.LstdFlags)
	args := &cicero.CLI{}
	parser, err := parseArgs(args)
	abort(parser, err)

	if args.Debug {
		logger.SetOutput(os.Stderr)
	}

	abort(parser, cicero.Run(parser, args))
}

func abort(parser *arg.Parser, err error) {
	switch err {
	case nil:
		return
	case arg.ErrHelp:
		parser.WriteHelp(os.Stderr)
		os.Exit(0)
	case arg.ErrVersion:
		fmt.Fprintln(os.Stdout, cicero.Version())
		os.Exit(0)
	default:
		fmt.Fprint(os.Stderr, err, "\n")
		os.Exit(1)
	}
}

func parseArgs(args *cicero.CLI) (*arg.Parser, error) {
	parser, err := arg.NewParser(arg.Config{}, args)
	if err != nil {
		return nil, err
	}

	err = parser.Parse(os.Args[1:])
	return parser, err
}
