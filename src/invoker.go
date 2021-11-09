package cicero

import (
	"context"
	"encoding/json"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"

	"cirello.io/oversight"
	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
)

type InvokerCmd struct {
	LiftbridgeAddr string `arg:"--liftbridge-addr" default:"127.0.0.1:9292"`
	Evaluator      string `arg:"--evaluator" default:"cicero-evaluator-nix"`
}

func (self InvokerCmd) init(invoker *Invoker) {
	if invoker.logger == nil {
		invoker.logger = log.New(os.Stderr, "invoker: ", log.LstdFlags)
	}
	if invoker.tree == nil {
		invoker.tree = oversight.New(oversight.WithSpecification(
			10,                    // number of restarts
			10*time.Minute,        // within this time period
			oversight.OneForOne(), // restart every task on its own
		))
	}
	if invoker.limiter == nil {
		// Increase priority of waiting goroutines every second.
		invoker.limiter = priority.NewLimiter(1, priority.WithDynamicPriority(1000))
	}
	if invoker.bridge == nil {
		bridge, err := service.LiftbridgeConnect(self.LiftbridgeAddr)
		if err != nil {
			invoker.logger.Fatalln(err.Error())
			return
		}
		invoker.bridge = &bridge
	}
	if invoker.evaluator == nil {
		e := NewEvaluator(self.Evaluator)
		invoker.evaluator = &e
	}
	if invoker.actionService == nil {
		s := service.NewActionService(DB)
		invoker.actionService = &s
	}
}

func (self InvokerCmd) Run() error {
	invoker := Invoker{}
	self.init(&invoker)

	if err := invoker.start(context.Background()); err != nil {
		return errors.WithMessage(err, "While running invoker")
	}

	for {
		time.Sleep(time.Hour)
	}
}

type Invoker struct {
	logger        *log.Logger
	tree          *oversight.Tree
	limiter       *priority.PriorityLimiter
	bridge        *liftbridge.Client
	evaluator     *Evaluator
	actionService *service.ActionService
}

func (self *Invoker) start(ctx context.Context) error {
	self.tree.Add(self.listenToInvoke)

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if err := self.tree.Start(ctx); err != nil {
		return errors.WithMessage(err, "While starting invoker supervisor")
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Invoker) listenToInvoke(ctx context.Context) error {
	self.logger.Println("Starting Invoker.listenToInvoke")

	if err := service.CreateStreams(self.logger, *self.bridge, []string{service.InvokeStreamName}); err != nil {
		return err
	}

	var offset int64
	if err := pgxscan.Get(
		context.Background(), DB, &offset,
		`SELECT COALESCE(MAX("offset") + 1, 0) FROM liftbridge_messages WHERE stream = $1`,
		service.InvokeStreamName,
	); err != nil {
		return errors.WithMessage(err, "Could not select last message offset")
	}

	self.logger.Printf("Subscribing to %s at offset %d\n", service.InvokeStreamName, offset)
	if err := (*self.bridge).Subscribe(
		ctx,
		service.InvokeStreamName,
		self.invokerSubscriber(ctx),
		liftbridge.StartAtOffset(offset),
		liftbridge.Partition(0),
	); err != nil {
		return errors.WithMessage(err, "failed to subscribe")
	}

	<-ctx.Done()
	self.logger.Println("context was cancelled")
	return nil
}

func (self *Invoker) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			self.logger.Fatalf("error in liftbridge message: %s", err.Error())
		}

		inputs := model.WorkflowCerts{}
		if err := json.Unmarshal(msg.Value(), &inputs); err != nil {
			self.logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)
			self.logger.Printf("Invalid JSON received, ignoring: %s\n", err)
			return
		}

		parts := strings.Split(msg.Subject(), ".")
		workflowName := parts[1]
		wfInstanceId, err := strconv.ParseUint(parts[2], 10, 64)
		if err != nil {
			self.logger.Printf("Invalid Workflow Instance ID received, ignoring: %s\n", msg.Subject())
			return
		}

		if err := service.InsertLiftbridgeMessage(self.logger, DB, msg); err != nil {
			return
		}

		if err := self.invokeWorkflow(ctx, workflowName, wfInstanceId, inputs); err != nil {
			self.logger.Println("Failed to invoke workflow", err)
		}
	}
}

func (self *Invoker) invokeWorkflow(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.WorkflowCerts) error {
	var version string
	if err := DB.QueryRow(
		context.Background(),
		`SELECT version FROM workflow_instances WHERE id = $1`,
		wfInstanceId,
	).Scan(&version); err != nil {
		return errors.WithMessage(err, "Could not find workflow instance with ID %d")
	}

	workflow, err := self.evaluator.EvaluateWorkflow(workflowName, &version, wfInstanceId, inputs)
	if err != nil {
		return errors.WithMessage(err, "Invalid Workflow Definition, ignoring")
	}

	for actionName, action := range workflow.Actions {
		if err := self.invokeWorkflowAction(ctx, workflowName, wfInstanceId, inputs, actionName, action); err != nil {
			return err
		}
	}

	return nil
}

func (self *Invoker) invokeWorkflowAction(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.WorkflowCerts, actionName string, action *model.WorkflowAction) error {
	self.limiter.Wait(context.Background(), priority.High)
	defer self.limiter.Finish()

	var instance *model.ActionInstance
	if inst, err := (*self.actionService).GetByNameAndWorkflowId(actionName, wfInstanceId); err != nil {
		if !pgxscan.NotFound(err) {
			return errors.WithMessage(err, "While getting last action instance")
		}
	} else {
		instance = &inst
	}

	self.logger.Printf("Checking runnability of %s: %v\n", actionName, action.IsRunnable())

	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if action.IsRunnable() {
			if instance == nil {
				instance = &model.ActionInstance{}
				instance.WorkflowInstanceId = wfInstanceId
				instance.Name = actionName
				instance.Certs = inputs

				err := (*self.actionService).Save(tx, instance)
				if err != nil {
					return errors.WithMessage(err, "Could not insert action instance")
				}
			} else {
				updatedAt := time.Now().UTC()
				instance.UpdatedAt = &updatedAt
				instance.Certs = inputs
				if err := (*self.actionService).Update(tx, instance.ID, *instance); err != nil {
					return errors.WithMessage(err, "Could not update action instance")
				}
			}

			actionInstanceId := instance.ID.String()
			action.Job.ID = &actionInstanceId

			if response, _, err := nomadClient.Jobs().Register(&action.Job, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to run action")
			} else if len(response.Warnings) > 0 {
				self.logger.Println(response.Warnings)
			}

		} else if instance != nil {
			if _, _, err := nomadClient.Jobs().Deregister(instance.ID.String(), false, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to stop action")
			}

			finished := time.Now().UTC()
			instance.FinishedAt = &finished

			if err := (*self.actionService).Update(tx, instance.ID, *instance); err != nil {
				return errors.WithMessage(err, "Failed to update action instance")
			}
		}
		return nil
	}); err != nil {
		return err
	}

	return nil
}
