package cicero

import (
	"encoding/json"
	"fmt"
	"github.com/input-output-hk/cicero/src/model"
	"log"
	"os"
)

type ShowCmd struct {
	WorkflowName string `arg:"--workflow,required"`
	ID           uint64 `arg:"--id" default:0`
	Inputs       string `arg:"--inputs" default:"{}"`
	logger       *log.Logger
	evaluator    Evaluator
}

func (cmd *ShowCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "show: ", log.LstdFlags)
	}
}

func (cmd *ShowCmd) Run() error {
	cmd.init()

	inputs := model.WorkflowCerts{}
	if err := json.Unmarshal([]byte(cmd.Inputs), &inputs); err != nil {
		return err
	}

	def, err := cmd.evaluator.EvaluateWorkflow(cmd.WorkflowName, cmd.ID, inputs)
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
