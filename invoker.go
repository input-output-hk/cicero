package main

import (
	"context"
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"

	"cirello.io/oversight"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

const invokeStreamName = "workflow.*.*.invoke"

var limiter *priority.PriorityLimiter
var invokerSupervisor *oversight.Tree

func init() {
	// Increase priority of waiting goroutines every second.
	limiter = priority.NewLimiter(1, priority.WithDynamicPriority(1000))

	invokerSupervisor = oversight.New(oversight.WithSpecification(
		10,                    // number of restarts
		10*time.Minute,        // within this time period
		oversight.OneForOne(), // restart every task on its own
	))
}

type InvokerCmd struct {
}

func runInvoker(args *InvokerCmd) error {
	invokerSupervisor.Add(invoker)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := invokerSupervisor.Start(ctx); err != nil {
		log.Fatal(err)
	}

	for {
		time.Sleep(1 * time.Hour)
	}
}

func invoker(ctx context.Context) error {
	client := connect([]string{invokeStreamName})
	defer client.Close()

	logger.Printf("Subscribing to %s\n", invokeStreamName)
	err := client.Subscribe(
		ctx,
		invokeStreamName,
		invokerSubscriber(ctx),
		liftbridge.StartAtLatestReceived(),
		liftbridge.Partition(0))

	if err != nil {
		return errors.WithMessage(err, "failed to subscribe")
	}

	<-ctx.Done()

	logger.Println("invoker was canceled")

	return nil
}

func invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
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

		err = invokeWorkflow(ctx, workflowName, id, inputs)
		if err != nil {
			logger.Println("Failed to invoke workflow")
		}
	}
}

func invokeWorkflow(ctx context.Context, workflowName string, workflowId uint64, inputs string) error {
	workflow, err := nixInstantiateWorkflow(workflowName, workflowId, inputs)
	if err != nil {
		return errors.WithMessage(err, "Invalid Workflow Definition, ignoring")
	}

	for taskName, task := range workflow.Tasks {
		fmt.Printf("Checking runnability of %s: %v\n", taskName, task.Run)
		if task.Run == nil {
			continue
		}

		err = invokeWorkflowTask(ctx, workflowName, workflowId, inputs, taskName, task)
		if err != nil {
			return err
		}
	}

	return nil
}

func invokeWorkflowTask(ctx context.Context, workflowName string, workflowId uint64, inputs string, taskName string, task workflowTask) error {
	limiter.Wait(context.Background(), priority.High)
	defer limiter.Finish()

	fmt.Printf("building %s.%s\n", workflowName, taskName)
	output, err := nixBuild(ctx, workflowName, workflowId, taskName, inputs)

	if err == nil {
		publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, workflowId), "workflow.*.*.cert", task.Success)
	} else {
		fmt.Println(string(output))
		publish(fmt.Sprintf("workflow.%s.%d.cert", workflowName, workflowId), "workflow.*.*.cert", task.Failure)
		return errors.WithMessage(err, "Failed to run nix-build")
	}

	return nil
}
