package main

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
)

type InvokerCmd struct {
}

func runInvoker(args *InvokerCmd) error {
	return invoker()
}

func invoker() error {
	client := connect([]string{"workflow.*.*.invoke"})
	defer client.Close()

	ctx := context.Background()
	err := client.Subscribe(
		ctx,
		"workflow.*.*.invoke",
		func(msg *liftbridge.Message, err error) {
			if err != nil {
				logger.Fatalf("error in liftbridge message: %s", err.Error())
			}
			inputs := string(msg.Value())
			logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)

			parts := strings.Split(msg.Subject(), ".")
			workflowName := parts[1]
			id, err := strconv.ParseUint(parts[2], 10, 64)
			if err != nil {
				logger.Printf("Invalid Workflow ID received, ignoring: %s\n", msg.Subject())
				return
			}

			workflow, err := nixInstantiateWorkflow(workflowName, id, inputs)
			if err != nil {
				logger.Printf("Invalid Workflow Definition, ignoring: %s\n", err)
				return
			}

			for taskName, task := range workflow.Tasks {
				fmt.Printf("Checking runnability of %s: %v\n", taskName, task.Run)
				if task.Run == nil {
					continue
				}

				go func(taskName string, task workflowTask) {
					fmt.Printf("building %s.%s\n", workflowName, taskName)
					output, err := nixBuild(workflowName, id, taskName, inputs)

					if err == nil {
						publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, id), "workflow.*.*.cert", task.Success)
					} else {
						fmt.Println(string(output))
						publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, id), "workflow.*.*.cert", task.Failure)
						fail(errors.WithMessage(err, "Failed to run nix-build"))
					}
				}(taskName, task)
			}
		}, liftbridge.StartAtEarliestReceived(), liftbridge.Partition(0))

	if err != nil {
		return errors.WithMessage(err, "failed to subscribe")
	}

	for {
		time.Sleep(60 * time.Second)
	}
}
