package cicero

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/google/uuid"
	"github.com/grafana/loki/pkg/loghttp"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/service"
	prometheus "github.com/prometheus/client_golang/api"
)

type Api struct {
	logger          *log.Logger
	workflowService service.WorkflowService
	evaluator       Evaluator
	prometheus      prometheus.Client
}

func (api *Api) init() {
	if api.logger == nil {
		api.logger = log.New(os.Stderr, "api: ", log.LstdFlags)
	}

	if api.prometheus == nil {
		client, err := prometheus.NewClient(prometheus.Config{
			Address: "http://127.0.0.1:3100",
		})
		if err != nil {
			fmt.Println(err.Error())
			os.Exit(1)
		}
		api.prometheus = client
	}
}

func (a *Api) WorkflowForInstance(wfName string, instanceId *uint64, logger *log.Logger) (def model.WorkflowDefinition, instance *model.WorkflowInstance, err error) {
	if instanceId != nil {
		var inst model.WorkflowInstance
		if inst, err = a.workflowService.GetById(*instanceId); err != nil {
			return
		} else {
			instance = &inst
		}

		def, err = a.evaluator.EvaluateWorkflow(instance.Name, &instance.Version, instance.ID, instance.Certs)
		return
	} else {
		def, err = a.Workflow(wfName, nil)
		return
	}
}

// TODO superfluous?
func (a *Api) Workflows() ([]string, error) {
	return a.evaluator.ListWorkflows(nil)
}

// TODO superfluous?
func (a *Api) Workflow(name string, version *string) (model.WorkflowDefinition, error) {
	return a.evaluator.EvaluateWorkflow(name, version, 0, model.WorkflowCerts{})
}

func (a *Api) Logs(nomadJobID uuid.UUID) ([]string, []string, error) {
	query := fmt.Sprintf(`{nomad_job_id="%s"}`, nomadJobID.String())
	linesToFetch := 10000
	// TODO: figure out the correct value for our infra, 5000 is the default
	// configuration in loki
	var limit int64 = 5000
	from := time.Unix(0, 0)
	stdout := []string{}
	stderr := []string{}

	// TODO: reduce allocations in this loop
	for {
		fmt.Println("fetching lines", linesToFetch, query, from)
		ctx, _ := context.WithTimeout(context.Background(), time.Second*5)
		req, err := http.NewRequest(
			"GET",
			a.prometheus.URL("/loki/api/v1/query_range", nil).String(),
			nil,
		)
		if err != nil {
			return nil, nil, err
		}

		q := req.URL.Query()
		q.Set("query", query)
		q.Set("limit", strconv.FormatInt(limit, 10))
		q.Set("start", strconv.FormatInt(from.UnixNano(), 10))
		q.Set("end", strconv.FormatInt(time.Now().UnixNano(), 10))
		q.Set("direction", "FORWARD")
		req.URL.RawQuery = q.Encode()

		done, body, err := a.prometheus.Do(ctx, req)
		if done.StatusCode/100 != 2 {
			return nil, nil, fmt.Errorf("Error response %d from Loki: %s (%v)", done.StatusCode, string(body), err)
		}

		response := loghttp.QueryResponse{}

		err = json.Unmarshal(body, &response)
		if err != nil {
			return nil, nil, err
		}

		streams, ok := response.Data.Result.(loghttp.Streams)
		if !ok {
			return nil, nil, fmt.Errorf("Unexpected loki result type: %s", response.Data.Result.Type())
		}

		if len(streams) == 0 {
			return stdout, stderr, nil
		}

		for _, stream := range streams {
			coll := stdout
			source, ok := stream.Labels.Map()["source"]
			if ok && source == "stderr" {
				coll = stderr
			}

			for _, entry := range stream.Entries {
				coll = append(coll, fmt.Sprintf("%s\t%s",
					entry.Timestamp.Format(time.RFC3339),
					entry.Line,
				))

				if ok && source == "stderr" {
					stderr = coll
				} else {
					stdout = coll
				}

				if len(coll) >= linesToFetch {
					return stdout, stderr, nil
				}
			}

			if int64(len(stream.Entries)) >= limit {
				from = stream.Entries[len(stream.Entries)-1].Timestamp
			} else if int64(len(stream.Entries)) < limit {
				return stdout, stderr, nil
			}
		}
	}
}
