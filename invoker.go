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
		"workflow.*.invoke",
		func(msg *liftbridge.Message, err error) {
			inputs := string(msg.Value())
			logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)

			parts := strings.Split(msg.Subject(), ".")
			id, err := strconv.ParseUint(parts[1], 10, 64)
			if err != nil {
				logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			instantiated, err := nixInstantiate(id, inputs)
			if err != nil {
				logger.Printf("Invalid Workflow Definition, ignoring: %s\n", err)
				return
			}

			for key, value := range *instantiated {
				if value.Run != nil {
					fmt.Printf("building %s\n", key)
					output, err := nixBuild(id, key, inputs)

					if err == nil {
						publish(fmt.Sprintf("workflow.%d.cert", id), "workflow.*.cert", value.Success)
					} else {
						fmt.Println(string(output))
						publish(fmt.Sprintf("workflow.%d.cert", id), "workflow.*.cert", value.Failure)
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

func nixBuild(id uint64, name string, inputs string) ([]byte, error) {
	return exec.Command(
		"nix-build",
		"--no-out-link",
		"--argstr", "id", strconv.FormatUint(id, 10),
		"--argstr", "inputsJSON", inputs,
		"./workflow.nix",
		"--attr", name+".run",
	).CombinedOutput()
}

type workflowDefinitions map[string]workflowDefinition
type workflowDefinition struct {
	Failure map[string]interface{} `json:"failure"`
	Success map[string]interface{} `json:"success"`
	Inputs  []string               `json:"inputs"`
	When    map[string]bool        `json:"when"`
	Run     *string                `json:"run"`
}

func nixInstantiate(id uint64, inputs string) (*workflowDefinitions, error) {
	output, err := exec.Command(
		"nix-instantiate",
		"--eval",
		"--strict",
		"--json",
		"./workflow.nix",
		"--argstr", "inputsJSON", inputs,
		"--argstr", "id", strconv.FormatUint(id, 10),
	).CombinedOutput()

	if err != nil {
		logger.Println(string(output))
		return nil, errors.WithMessage(err, "Failed to run nix-instantiate")
	}

	result := &workflowDefinitions{}
	err = json.Unmarshal(output, result)
	return result, err
}
