package service

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/grafana/loki/pkg/loghttp"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
)

type ActionService interface {
	GetById(uuid.UUID) (model.ActionInstance, error)
	GetByNameAndWorkflowId(name string, workflowId uint64) (model.ActionInstance, error)
	GetAll() ([]*model.ActionInstance, error)
	Save(pgx.Tx, *model.ActionInstance) error
	Update(pgx.Tx, model.ActionInstance) error
	JobLogs(uuid.UUID) (*LokiOutput, error)
	ActionLogs(allocId string, taskGroup string) (*LokiOutput, error)
}

type ActionServiceImpl struct {
	logger           *log.Logger
	actionRepository repository.ActionRepository
	prometheus       prometheus.Client
}

func NewActionService(db *pgxpool.Pool, prometheusAddr string) ActionService {
	impl := ActionServiceImpl{
		logger:           log.New(os.Stderr, "ActionService: ", log.LstdFlags),
		actionRepository: repository.NewActionRepository(db),
	}

	if prom, err := prometheus.NewClient(prometheus.Config{
		Address: prometheusAddr,
	}); err != nil {
		impl.logger.Fatal(err.Error())
	} else {
		impl.prometheus = prom
	}

	return &impl
}

func (self *ActionServiceImpl) GetById(id uuid.UUID) (action model.ActionInstance, err error) {
	log.Printf("Get Action by id %s", id)
	action, err = self.actionRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Couldn't select existing Action for id: %s", id)
	}
	return
}

func (self *ActionServiceImpl) GetByNameAndWorkflowId(name string, workflowId uint64) (action model.ActionInstance, err error) {
	log.Printf("Get Action by name %s and workflow_instance_id %d", name, workflowId)
	action, err = self.actionRepository.GetByNameAndWorkflowId(name, workflowId)
	if err != nil && !pgxscan.NotFound(err) {
		err = errors.WithMessagef(err, "Couldn't select existing Action for name %s and workflow_instance_id %d", name, workflowId)
	}
	return
}

func (self *ActionServiceImpl) GetAll() ([]*model.ActionInstance, error) {
	log.Printf("Get all Actions")
	return self.actionRepository.GetAll()
}

func (self *ActionServiceImpl) Save(tx pgx.Tx, action *model.ActionInstance) error {
	log.Printf("Saving new Action %#v", action)
	if err := self.actionRepository.Save(tx, action); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Action")
	}
	log.Printf("Created Action %#v", action)
	return nil
}

func (self *ActionServiceImpl) Update(tx pgx.Tx, action model.ActionInstance) error {
	log.Printf("Update Action %#v", action)
	if err := self.actionRepository.Update(tx, action); err != nil {
		return errors.WithMessagef(err, "Couldn't update Action with id: %s", action.ID)
	}
	log.Printf("Updated Action %#v", action)
	return nil
}

type LokiLine struct {
	Time time.Time
	Text string
}

type LokiOutput struct {
	Stderr []LokiLine
	Stdout []LokiLine
}

func (self *ActionServiceImpl) JobLogs(nomadJobID uuid.UUID) (*LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_job_id=%q}`,
		nomadJobID.String(),
	))
}

func (self *ActionServiceImpl) ActionLogs(allocID string, taskGroup string) (*LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_alloc_id=%q,nomad_task_group=%q}`,
		allocID,
		taskGroup,
	))
}

func (self *ActionServiceImpl) LokiQueryRange(query string) (*LokiOutput, error) {
	linesToFetch := 10000
	// TODO: figure out the correct value for our infra, 5000 is the default
	// configuration in loki
	var limit int64 = 5000
	from := time.Unix(0, 0)
	output := &LokiOutput{
		Stdout: []LokiLine{},
		Stderr: []LokiLine{},
	}

	// TODO: reduce allocations in this loop
	for {
		ctx, _ := context.WithTimeout(context.Background(), time.Second*5)
		req, err := http.NewRequest(
			"GET",
			self.prometheus.URL("/loki/api/v1/query_range", nil).String(),
			http.NoBody,
		)
		if err != nil {
			return output, err
		}

		q := req.URL.Query()
		q.Set("query", query)
		q.Set("limit", strconv.FormatInt(limit, 10))
		q.Set("start", strconv.FormatInt(from.UnixNano(), 10))
		q.Set("end", strconv.FormatInt(time.Now().UnixNano(), 10))
		q.Set("direction", "FORWARD")
		req.URL.RawQuery = q.Encode()

		fmt.Println(req.URL.Query())
		fmt.Println(req)

		done, body, err := self.prometheus.Do(ctx, req)
		if err != nil {
			return output, errors.WithMessage(err, "Failed to talk with loki")
		}

		if done.StatusCode/100 != 2 {
			return output, fmt.Errorf("Error response %d from Loki: %s (%v)", done.StatusCode, string(body), err)
		}

		response := loghttp.QueryResponse{}

		err = json.Unmarshal(body, &response)
		if err != nil {
			return output, err
		}

		streams, ok := response.Data.Result.(loghttp.Streams)
		if !ok {
			return output, fmt.Errorf("Unexpected loki result type: %s", response.Data.Result.Type())
		}

		if len(streams) == 0 {
			return output, nil
		}

		for _, stream := range streams {
			source, ok := stream.Labels.Map()["source"]

			for _, entry := range stream.Entries {
				if ok && source == "stderr" {
					output.Stderr = append(output.Stderr, LokiLine{entry.Timestamp, entry.Line})
				} else {
					output.Stdout = append(output.Stdout, LokiLine{entry.Timestamp, entry.Line})
				}

				if (len(output.Stdout) + len(output.Stderr)) >= linesToFetch {
					return output, nil
				}
			}

			if int64(len(stream.Entries)) >= limit {
				from = stream.Entries[len(stream.Entries)-1].Timestamp
			} else if int64(len(stream.Entries)) < limit {
				return output, nil
			}
		}
	}
}
