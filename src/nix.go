package cicero

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

func nixInstantiate(logger *log.Logger, attr string, id uint64, inputs string) (*WorkflowDefinitions, error) {
	cmd := exec.Command(
		"nix-instantiate",
		"--eval",
		"--strict",
		"--json",
		"./lib.nix",
		"--argstr", "inputs", inputs,
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--attr", attr,
	)

	fmt.Printf("running %s\n", strings.Join(cmd.Args, " "))
	output, err := cmd.CombinedOutput()

	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "Failed to run nix-instantiate")
	}

	result := &WorkflowDefinitions{}
	err = json.Unmarshal(output, result)
	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflowDefinitions")
	}
	return result, nil
}

func nixInstantiateWorkflow(logger *log.Logger, workflowName string, id uint64, inputs string) (*WorkflowDefinition, error) {
	defs, err := nixInstantiate(logger, "workflows", id, inputs)
	if err != nil {
		return nil, err
	}

	return (*defs)[workflowName], nil
}
