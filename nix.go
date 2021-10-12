package main

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

func nixBuild(workflowName string, id uint64, name string, inputs string) ([]byte, error) {
	return exec.Command(
		"nix-build",
		"--no-out-link",
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--argstr", "inputsJSON", inputs,
		"./lib.nix",
		"--attr", fmt.Sprintf("workflows.%s.tasks.%s.run", workflowName, name),
	).CombinedOutput()
}

type workflowDefinitions map[string]*workflowDefinition
type workflowDefinition struct {
	Name    string                  `json:"name"`
	Version uint64                  `json:"version"`
	Meta    map[string]interface{}  `json:"meta"`
	Tasks   map[string]workflowTask `json:"tasks"`
}
type workflowTask struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Run     *string                `json:"run"`
}

func nixInstantiate(attr string, id uint64, inputs string) (*workflowDefinitions, error) {
	cmd := exec.Command(
		"nix-instantiate",
		"--eval",
		"--strict",
		"--json",
		"./lib.nix",
		"--argstr", "inputsJSON", inputs,
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--attr", attr,
	)

	fmt.Printf("running nix-instantiate %s\n", strings.Join(cmd.Args, " "))
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

func nixInstantiateWorkflow(workflowName string, id uint64, inputs string) (*workflowDefinition, error) {
	defs, err := nixInstantiate("workflows", id, inputs)
	if err != nil {
		return nil, err
	}

	return (*defs)[workflowName], nil
}
