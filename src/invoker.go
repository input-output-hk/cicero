package cicero

import (
	"context"
	"encoding/json"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"cirello.io/oversight"
	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge"
	"github.com/pkg/errors"
	"github.com/vivek-ng/concurrency-limiter/priority"
	"gopkg.in/yaml.v3"
)

const invokeStreamName = "workflow.*.*.invoke"

type InvokerCmd struct {
	logger        	*log.Logger
	tree          	*oversight.Tree
	limiter       	*priority.PriorityLimiter
	bridge        	liftbridge.Client
	evaluator     	Evaluator
	actionService 	service.ActionService
	workflowService service.WorkflowService
}

func (cmd *InvokerCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "invoker: ", log.LstdFlags)
	}

	if cmd.tree == nil {
		cmd.tree = oversight.New(oversight.WithSpecification(
			10,                    // number of restarts
			10*time.Minute,        // within this time period
			oversight.OneForOne(), // restart every task on its own
		))
	}
	if cmd.actionService == nil {
		cmd.actionService = service.NewActionService(DB)
	}
	if cmd.workflowService == nil {
		cmd.workflowService = service.NewWorkflowService(DB)
	}
	if cmd.limiter == nil {
		// Increase priority of waiting goroutines every second.
		cmd.limiter = priority.NewLimiter(1, priority.WithDynamicPriority(1000))
	}
}

func (cmd *InvokerCmd) Run() error {
	cmd.init()
	cmd.tree.Add(cmd.listenToInvoke)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := cmd.tree.Start(ctx); err != nil {
		return err
	}

	for {
		time.Sleep(1 * time.Hour)
	}
}

func (cmd *InvokerCmd) start(ctx context.Context) error {
	if err := cmd.listenToInvoke(ctx); err != nil {
		return err
	}

	<-ctx.Done()
	cmd.logger.Println("context was cancelled")
	return nil
}

func (cmd *InvokerCmd) listenToInvoke(ctx context.Context) error {
	cmd.init()
	cmd.logger.Println("Starting Invoker.listenToInvoke")

	err := service.CreateStreams(cmd.logger, cmd.bridge, []string{invokeStreamName})
	if err != nil {
		return err
	}

	var offset int64
	pgxscan.Get(context.Background(), DB, &offset,
		`SELECT COALESCE(MAX("offset") + 1, 0) FROM liftbridge_messages WHERE stream = $1`,
		invokeStreamName)

	cmd.logger.Printf("Subscribing to %s at offset %d\n", invokeStreamName, offset)
	err = cmd.bridge.Subscribe(
		ctx,
		invokeStreamName,
		cmd.invokerSubscriber(ctx),
		liftbridge.StartAtOffset(offset),
		liftbridge.Partition(0))

	if err != nil {
		return errors.WithMessage(err, "failed to subscribe")
	}

	return nil
}

func (cmd *InvokerCmd) invokerSubscriber(ctx context.Context) func(*liftbridge.Message, error) {
	return func(msg *liftbridge.Message, err error) {
		if err != nil {
			cmd.logger.Fatalf("error in liftbridge message: %s", err.Error())
		}

		inputs := model.WorkflowCerts{}
		if err := json.Unmarshal(msg.Value(), &inputs); err != nil {
			cmd.logger.Println(msg.Timestamp(), msg.Offset(), string(msg.Key()), inputs)
			cmd.logger.Printf("Invalid JSON received, ignoring: %s\n", err)
			return
		}

		parts := strings.Split(msg.Subject(), ".")
		workflowName := parts[1]
		wfInstanceId, err := strconv.ParseUint(parts[2], 10, 64)
		if err != nil {
			cmd.logger.Printf("Invalid Workflow Instance ID received, ignoring: %s\n", msg.Subject())
			return
		}

		if err := service.InsertLiftbridgeMessage(cmd.logger, DB, msg); err != nil {
			return
		}

		if err := cmd.invokeWorkflow(ctx, workflowName, wfInstanceId, inputs); err != nil {
			cmd.logger.Println("Failed to invoke workflow", err)
		}
	}
}

func (cmd *InvokerCmd) invokeWorkflow(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.WorkflowCerts) error {
	wf, err := cmd.workflowService.GetById(wfInstanceId)
	if err != nil {
		return errors.WithMessage(err, "Could not find workflow instance with ID %d")
	}

	workflow, err := cmd.evaluator.EvaluateWorkflow(workflowName, &wf.Version, wfInstanceId, inputs)
	if err != nil {
		return errors.WithMessage(err, "Invalid Workflow Definition, ignoring")
	}

	for actionName, action := range workflow.Actions {
		if err := cmd.invokeWorkflowAction(ctx, workflowName, wfInstanceId, inputs, actionName, action); err != nil {
			return err
		}
	}

	return nil
}

func (cmd *InvokerCmd) invokeWorkflowAction(ctx context.Context, workflowName string, wfInstanceId uint64, inputs model.WorkflowCerts, actionName string, action *model.WorkflowAction) error {
	cmd.limiter.Wait(context.Background(), priority.High)
	defer cmd.limiter.Finish()

	instance, err := cmd.actionService.GetByNameAndWorkflowId(actionName, wfInstanceId)
	if err != nil {
		if !pgxscan.NotFound(err) {
			return errors.WithMessage(err, "While getting last action instance")
		}
		instance = nil
	}

	cmd.logger.Printf("Checking runnability of %s: %v\n", actionName, action.IsRunnable())

	if err := DB.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if action.IsRunnable() {
			if err := addLogging(&action.Job); err != nil {
				return err
			}
			if instance == nil {
				instance = &model.ActionInstance{}
				instance.WorkflowInstanceId = wfInstanceId
				instance.Name = actionName
				instance.Certs = inputs

				err := cmd.actionService.Save(tx, instance)
				if err != nil {
					return errors.WithMessage(err, "Could not insert action instance")
				}
			} else {
				updatedAt := time.Now().UTC()
				instance.UpdatedAt = &updatedAt
				instance.Certs = inputs
				if err := cmd.actionService.Update(tx, instance.ID, instance); err != nil {
					return errors.WithMessage(err, "Could not update action instance")
				}
			}

			actionInstanceId := instance.ID.String()
			action.Job.ID = &actionInstanceId

			if response, _, err := nomadClient.Jobs().Register(&action.Job, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to run action")
			} else if len(response.Warnings) > 0 {
				cmd.logger.Println(response.Warnings)
			}

		} else if instance != nil {
			if _, _, err := nomadClient.Jobs().Deregister(instance.ID.String(), false, &nomad.WriteOptions{}); err != nil {
				return errors.WithMessage(err, "Failed to stop action")
			}

			finished := time.Now().UTC()
			instance.FinishedAt = &finished

			if err := cmd.actionService.Update(tx, instance.ID, instance); err != nil {
				return errors.WithMessage(err, "Failed to update action instance")
			}
		}
		return nil
	}); err != nil {
		return err
	}

	return nil
}

func addLogging(job *nomad.Job) error {
	pStr := func(v string) *string { return &v }
	pInt := func(v int) *int { return &v }

	cfg, err := yaml.Marshal(map[string]interface{}{
		"server": map[string]int{
			"http_listen_port": 0,
			"grpc_listen_port": 0,
		},
		"positions": map[string]string{"filename": "/local/positions.yaml"},
		"client":    map[string]string{"url": "http://172.16.0.20:3100/loki/api/v1/path"},
		"scrape_configs": []map[string]interface{}{{
			"job_name":        `{{ env "NOMAD_JOB_NAME" }}-{{ env "NOMAD_ALLOC_INDEX" }}`,
			"pipeline_stages": nil,
			"static_configs": []map[string]interface{}{{
				"labels": map[string]string{
					"nomad_alloc_id":      `{{ env "NOMAD_ALLOC_ID" }}`,
					"nomad_alloc_index":   `{{ env "NOMAD_ALLOC_INDEX" }}`,
					"nomad_alloc_name":    `{{ env "NOMAD_ALLOC_NAME" }}`,
					"nomad_dc":            `{{ env "NOMAD_DC" }}`,
					"nomad_group_name":    `{{ env "NOMAD_GROUP_NAME" }}`,
					"nomad_job_id":        `{{ env "NOMAD_JOB_ID" }}`,
					"nomad_job_name":      `{{ env "NOMAD_JOB_NAME" }}`,
					"nomad_job_parent_id": `{{ env "NOMAD_JOB_PARENT_ID" }}`,
					"nomad_namespace":     `{{ env "NOMAD_NAMESPACE" }}`,
					"nomad_region":        `{{ env "NOMAD_REGION" }}`,
					"__path__":            "/alloc/logs/*.std*.[0-9]*",
				},
			}},
		}},
	})
	if err != nil {
		return errors.WithMessage(err, "while marshaling promtail config")
	}

	for _, tg := range job.TaskGroups {
		tg.Tasks = append(tg.Tasks, &nomad.Task{
			Name:   "promtail",
			Driver: "nix",
			Lifecycle: &nomad.TaskLifecycle{
				Hook:    "prestart",
				Sidecar: true,
			},
			Resources: &nomad.Resources{
				CPU:      pInt(100),
				MemoryMB: pInt(100),
			},
			Config: map[string]interface{}{
				"packages": []string{"github:nixos/nixpkgs/nixos-21.05#grafana-loki"},
				"command":  []string{"/bin/promtail", "-config.file", "local/config.yaml"},
			},
			Templates: []*nomad.Template{{
				DestPath:     pStr("local/config.yaml"),
				EmbeddedTmpl: pStr(string(cfg)),
			}},
		})
	}

	return nil
}
