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

	"github.com/google/uuid"
	"github.com/grafana/loki/pkg/loghttp"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type RunService interface {
	GetById(uuid.UUID) (domain.Run, error)
	GetAll() ([]*domain.Run, error)
	Save(pgx.Tx, *domain.Run) error
	Update(pgx.Tx, *domain.Run) error
	JobLogs(uuid.UUID) (*domain.LokiOutput, error)
	RunLogs(allocId string, taskGroup string) (*domain.LokiOutput, error)
}

type runService struct {
	logger        *log.Logger
	runRepository repository.RunRepository
	prometheus    prometheus.Client
}

func NewRunService(db config.PgxIface, prometheusAddr string) RunService {
	impl := runService{
		logger:        log.New(os.Stderr, "RunService: ", log.LstdFlags),
		runRepository: persistence.NewRunRepository(db),
	}

	if prom, err := prometheus.NewClient(prometheus.Config{
		Address: prometheusAddr,
	}); err != nil {
		impl.logger.Fatal(err.Error())
		return nil
	} else {
		impl.prometheus = prom
	}

	return &impl
}

func (self *runService) GetById(id uuid.UUID) (run domain.Run, err error) {
	self.logger.Printf("Getting Run by ID %s", id)
	run, err = self.runRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select existing Run by ID: %s", id)
	}
	return
}

func (self *runService) GetAll() ([]*domain.Run, error) {
	self.logger.Printf("Getting all Runs")
	return self.runRepository.GetAll()
}

func (self *runService) Save(tx pgx.Tx, run *domain.Run) error {
	self.logger.Println("Saving new Run")
	if err := self.runRepository.Save(tx, run); err != nil {
		return errors.WithMessagef(err, "Could not insert Run")
	}
	self.logger.Printf("Created Run %s", run.NomadJobID)
	return nil
}

func (self *runService) Update(tx pgx.Tx, run *domain.Run) error {
	self.logger.Printf("Update Run ID %s", run.NomadJobID)
	if err := self.runRepository.Update(tx, run); err != nil {
		return errors.WithMessagef(err, "Could not update Run ID: %s", run.NomadJobID)
	}
	self.logger.Printf("Updated Run ID %s", run.NomadJobID)
	return nil
}

func (self *runService) JobLogs(nomadJobID uuid.UUID) (*domain.LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_job_id=%q}`,
		nomadJobID.String(),
	))
}

func (self *runService) RunLogs(allocID, taskGroup string) (*domain.LokiOutput, error) {
	return self.LokiQueryRange(fmt.Sprintf(
		`{nomad_alloc_id=%q,nomad_task_group=%q}`,
		allocID,
		taskGroup,
	))
}

func (self *runService) LokiQueryRange(query string) (*domain.LokiOutput, error) {
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
					output.Stderr = append(output.Stderr, domain.LokiLine{Time: entry.Timestamp, Text: entry.Line})
				} else {
					output.Stdout = append(output.Stdout, domain.LokiLine{Time: entry.Timestamp, Text: entry.Line})
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
