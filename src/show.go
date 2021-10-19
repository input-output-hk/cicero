package cicero

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

func (cmd *ShowCmd) Run() error {
	cmd.init()

	def, err := nixInstantiateWorkflow(cmd.logger, cmd.WorkflowName, cmd.ID, cmd.Inputs)
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
