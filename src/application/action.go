package application

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
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
)

type ActionService interface {
	GetById(uuid.UUID) (domain.ActionInstance, error)
	GetByNameAndWorkflowId(name string, workflowId uint64) (domain.ActionInstance, error)
	GetAll() ([]*domain.ActionInstance, error)
	Save(pgx.Tx, *domain.ActionInstance) error
	Update(pgx.Tx, domain.ActionInstance) error
	JobLogs(uuid.UUID) (*domain.LokiOutput, error)
	ActionLogs(allocId string, taskGroup string) (*domain.LokiOutput, error)
}

type actionService struct {
	logger           *log.Logger
	actionRepository repository.ActionRepository
	prometheus       prometheus.Client
}

func NewActionService(db *pgxpool.Pool, prometheusAddr string) ActionService {
	impl := actionService{
		logger:           log.New(os.Stderr, "ActionService: ", log.LstdFlags),
		actionRepository: persistence.NewActionRepository(db),
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

func (self *actionService) GetById(id uuid.UUID) (action domain.ActionInstance, err error) {
	self.logger.Printf("Get Action by id %s", id)
	action, err = self.actionRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Couldn't select existing Action for id: %s", id)
	}
	return
}

func (self *actionService) GetByNameAndWorkflowId(name string, workflowId uint64) (action domain.ActionInstance, err error) {
	self.logger.Printf("Get Action by name %s and workflow_instance_id %d", name, workflowId)
	action, err = self.actionRepository.GetByNameAndWorkflowId(name, workflowId)
	if err != nil && !pgxscan.NotFound(err) {
		err = errors.WithMessagef(err, "Couldn't select existing Action for name %s and workflow_instance_id %d", name, workflowId)
	}
	return
}

func (self *actionService) GetAll() ([]*domain.ActionInstance, error) {
	self.logger.Printf("Get all Actions")
	return self.actionRepository.GetAll()
}

func (self *actionService) Save(tx pgx.Tx, action *domain.ActionInstance) error {
	self.logger.Printf("Saving new Action named %s", action.Name)
	if err := self.actionRepository.Save(tx, action); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Action")
	}
	self.logger.Printf("Created Action %s", action.ID)
	return nil
}

func (self *actionService) Update(tx pgx.Tx, action domain.ActionInstance) error {
	self.logger.Printf("Update Action %s", action.ID.String())
	if err := self.actionRepository.Update(tx, action); err != nil {
		return errors.WithMessagef(err, "Couldn't update Action with id: %s", action.ID)
	}
	self.logger.Printf("Updated Action %s", action.ID.String())
	return nil
}

func (self *actionService) JobLogs(nomadJobID uuid.UUID) (*domain.LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_job_id=%q}`,
		nomadJobID.String(),
	))
}

func (self *actionService) ActionLogs(allocID, taskGroup string) (*domain.LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_alloc_id=%q,nomad_task_group=%q}`,
		allocID,
		taskGroup,
	))
}

func (self *actionService) LokiQueryRange(query string) (*domain.LokiOutput, error) {
	linesToFetch := 10000
	// TODO: figure out the correct value for our infra, 5000 is the default
	// configuration in loki
	var limit int64 = 5000
	from := time.Unix(0, 0)
	output := &domain.LokiOutput{
		Stdout: []domain.LokiLine{},
		Stderr: []domain.LokiLine{},
	}

	// TODO: reduce allocations in this loop
	for {
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

		done, body, err := self.prometheus.Do(context.Background(), req)
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
					output.Stderr = append(output.Stderr, domain.LokiLine{entry.Timestamp, entry.Line})
				} else {
					output.Stdout = append(output.Stdout, domain.LokiLine{entry.Timestamp, entry.Line})
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
