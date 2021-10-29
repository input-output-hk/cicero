package cicero

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"

	"github.com/pkg/errors"
)

type Evaluator struct {
	Command string
	logger *log.Logger
}

func NewEvaluator(command string) Evaluator {
	return Evaluator{
		Command: command,
		logger: log.New(os.Stderr, "evaluator: ", log.LstdFlags),
	}
}

func (e *Evaluator) EvaluateWorkflow(name string, id uint64, inputs WorkflowCerts) (WorkflowDefinition, error) {
	var def WorkflowDefinition

	inputsJson, err := json.Marshal(inputs)
	if err != nil {
		return def, err
	}

	cmd := exec.Command(
		e.Command,
		"eval",
	)
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_NAME=" + name)
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_INSTANCE_ID=" + fmt.Sprintf("%d", id))
	cmd.Env = append(cmd.Env, "CICERO_WORKFLOW_INPUTS=" + string(inputsJson))

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

func (e *Evaluator) ListWorkflows() ([]string, error) {
	var names []string

	cmd := exec.Command(
		e.Command,
		"list",
	)

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
