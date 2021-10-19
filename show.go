package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
)

type ShowCmd struct {
	WorkflowName string `arg:"--workflow,required"`
	ID           uint64 `arg:"--id" default:0`
	Inputs       string `arg:"--inputs" default:"{}"`
	logger       *log.Logger
}

func (cmd *ShowCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "show: ", log.LstdFlags)
	}
}

func runShow(args *ShowCmd) error {
	return args.show()
}

func (args *ShowCmd) show() error {
	args.init()

	def, err := nixInstantiateWorkflow(args.logger, args.WorkflowName, args.ID, args.Inputs)
	if err != nil {
		return err
	}

	prettyJSON, err := json.MarshalIndent(def, "", "    ")
	if err != nil {
		log.Fatal("Failed to generate json", err)
	}
	fmt.Printf("%s\n", string(prettyJSON))

	return nil
}
