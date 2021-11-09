package cicero

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"

	"github.com/input-output-hk/cicero/src/model"
	"github.com/pkg/errors"
)

type Evaluator struct {
	Command string
	logger  *log.Logger
}

func NewEvaluator(command string) Evaluator {
	return Evaluator{
		Command: command,
		logger:  log.New(os.Stderr, "evaluator: ", log.LstdFlags),
	}
}

func (e *Evaluator) EvaluateWorkflow(name string, version *string, id uint64, inputs model.WorkflowCerts) (model.WorkflowDefinition, error) {
	var def model.WorkflowDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, errors.WithMessagef(err, "Could not marshal workflow inputs to json: %s", inputs)
	}

	cmd := exec.Command(
		e.Command,
		"eval",
	)
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_NAME="+name)
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_INSTANCE_ID="+fmt.Sprintf("%d", id))
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_INPUTS="+string(inputsJson))
	if version != nil {
		cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_VERSION="+*version)
	}

	e.logger.Printf("running %s\n", strings.Join(cmd.Args, " "))
	output, err := cmd.Output()

	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "Failed to evaluate workflow")
	}

	err = json.Unmarshal(output, &def)
	if err != nil {
		e.logger.Println(string(output))
		return def, errors.WithMessage(err, "While unmarshaling WorkflowDefinition")
	}

	return def, nil
}

func (e *Evaluator) ListWorkflows(version *string) ([]string, error) {
	var names []string

	cmd := exec.Command(
		e.Command,
		"list",
	)
	if version != nil {
		cmd.Env = append(os.Environ(), "CICERO_WORKFLOW_VERSION="+*version)
	}

	e.logger.Printf("running %s\n", strings.Join(cmd.Args, " "))
	output, err := cmd.Output()

	if err != nil {
		return nil, errors.WithMessage(err, "Failed to list workflows")
	}

	err = json.Unmarshal(output, &names)
	if err != nil {
		e.logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflow names")
	}

	return names, nil
}
