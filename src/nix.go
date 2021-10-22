package cicero

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	nomad "github.com/hashicorp/nomad/api"
)

type workflowDefinitions map[string]*workflowDefinition

type workflowDefinition struct {
	Name    string                  `json:"name"`
	Version uint64                  `json:"version"`
	Meta    map[string]interface{}  `json:"meta"`
	Steps   map[string]workflowStep `json:"steps"`
}

type workflowStep struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Job     *nomad.Job             `json:"job"`
}

func nixInstantiate(logger *log.Logger, attr string, id uint64, inputs string) (*workflowDefinitions, error) {
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

	result := &workflowDefinitions{}
	err = json.Unmarshal(output, result)
	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflowDefinitions")
	}
	return result, nil
}

func nixInstantiateWorkflow(logger *log.Logger, workflowName string, id uint64, inputs string) (*workflowDefinition, error) {
	defs, err := nixInstantiate(logger, "workflows", id, inputs)
	if err != nil {
		return nil, err
	}

	return (*defs)[workflowName], nil
}
