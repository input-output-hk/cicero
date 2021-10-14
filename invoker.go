package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
)

type InvokerCmd struct {
}

func runInvoker(args *InvokerCmd) error {
	invoker()
	return nil
}

func invoker() {
	client := connect([]string{"workflow.*.invoke"})
	defer client.Close()

	ctx := context.Background()
	err := client.Subscribe(
		ctx,
		"workflow.*.*.invoke",
		func(msg *liftbridge.Message, err error) {
			inputs := string(msg.Value())
			logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)

			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]
			id, err := strconv.ParseUint(parts[2], 10, 64)
			if err != nil {
				logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			workflow, err := nixInstantiate(workflowName, id, inputs)
			if err != nil {
				logger.Printf("Invalid Workflow Definition, ignoring: %s\n", err)
				return
			}

			for taskName, task := range workflow.Tasks {
				if task.Run != nil {
					fmt.Printf("building %s.%s\n", workflowName, taskName)
					output, err := nixBuild(workflowName, id, taskName, inputs)

					if err == nil {
						publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, id), "workflow.*.*.cert", task.Success)
					} else {
						fmt.Println(string(output))
						publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, id), "workflow.*.*.cert", task.Failure)
						fail(errors.WithMessage(err, "Failed to run nix-build"))
					}
				}
			}
		}, liftbridge.StartAtEarliestReceived(), liftbridge.Partition(0))

	fail(errors.WithMessage(err, "failed to subscribe"))

	for {
		time.Sleep(10 * time.Second)
	}
}

func nixBuild(workflowName string, id uint64, name string, inputs string) ([]byte, error) {
	return exec.Command(
		"nix-build",
		"--no-out-link",
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--argstr", "inputsJSON", inputs,
		"./lib.nix",
		"--attr", fmt.Sprintf("workflows.%s.%s.run", workflowName, name),
	).CombinedOutput()
}

type workflowDefinitions map[string]workflowDefinition
type workflowDefinition struct {
	Name  string
	Meta  map[string]interface{}
	Tasks map[string]workflowTask
}
type workflowTask struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Run     *string                `json:"run"`
}

func nixInstantiate(workflowName string, id uint64, inputs string) (*workflowDefinition, error) {
	output, err := exec.Command(
		"nix-instantiate",
		"--eval",
		"--strict",
		"--json",
		"./lib.nix",
		"--argstr", "inputsJSON", inputs,
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--attr", fmt.Sprintf(`workflows."%s"`, workflowName),
	).CombinedOutput()

	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "Failed to run nix-instantiate")
	}

	result := &workflowDefinition{}
	err = json.Unmarshal(output, result)
	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "While unmarshaling workflowDefinition")
	}
	return result, nil
}
