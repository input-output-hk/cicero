package service

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"sort"
	"strconv"
	"time"

	"github.com/google/uuid"
	"github.com/grafana/loki/pkg/loghttp"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"
	prometheus "github.com/prometheus/client_golang/api"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type RunService interface {
	WithQuerier(config.PgxIface) RunService

	GetByNomadJobId(uuid.UUID) (domain.Run, error)
	GetByNomadJobIdWithLock(uuid.UUID, string) (domain.Run, error)
	GetInputFactIdsByNomadJobId(uuid.UUID) (repository.RunInputFactIds, error)
	GetOutputByNomadJobId(uuid.UUID) (domain.RunOutput, error)
	GetByActionId(uuid.UUID, *repository.Page) ([]*domain.Run, error)
	GetLatestByActionId(uuid.UUID) (domain.Run, error)
	GetAll(*repository.Page) ([]*domain.Run, error)
	GetByInputFactIds([]*uuid.UUID, bool, *repository.Page) ([]*domain.Run, error)
	Save(*domain.Run, map[string]interface{}, *domain.RunOutput) error
	Update(*domain.Run) error
	End(*domain.Run) error
	Cancel(*domain.Run) error
	JobLogs(id uuid.UUID, start time.Time, end *time.Time) (domain.LokiLog, error)
	RunLogs(allocId, taskGroup, taskName string, start time.Time, end *time.Time) (domain.LokiLog, error)
}

type runService struct {
	logger              zerolog.Logger
	runRepository       repository.RunRepository
	runOutputRepository repository.RunOutputRepository
	prometheus          prometheus.Client
	nomadClient         application.NomadClient
	db                  config.PgxIface
}

func NewRunService(db config.PgxIface, prometheusAddr string, nomadClient application.NomadClient, logger *zerolog.Logger) RunService {
	impl := runService{
		logger:              logger.With().Str("component", "RunService").Logger(),
		runRepository:       persistence.NewRunRepository(db),
		runOutputRepository: persistence.NewRunOutputRepository(db),
		nomadClient:         nomadClient,
		db:                  db,
	}

	if prom, err := prometheus.NewClient(prometheus.Config{
		Address: prometheusAddr,
	}); err != nil {
		impl.logger.Fatal().Err(err).Msg("Failed to create new prometheus client")
		return nil
	} else {
		impl.prometheus = prom
	}

	return &impl
}

func (self *runService) WithQuerier(querier config.PgxIface) RunService {
	return &runService{
		logger:              self.logger,
		runRepository:       self.runRepository.WithQuerier(querier),
		runOutputRepository: self.runOutputRepository.WithQuerier(querier),
		prometheus:          self.prometheus,
		nomadClient:         self.nomadClient,
		db:                  querier,
	}
}

func (self *runService) GetByNomadJobId(id uuid.UUID) (run domain.Run, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Msg("Getting Run by Nomad Job ID")
	run, err = self.runRepository.GetByNomadJobId(id)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q", id)
	return
}

func (self *runService) GetByNomadJobIdWithLock(id uuid.UUID, lock string) (run domain.Run, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Str("lock", lock).Msg("Getting Run by Nomad Job ID with lock")
	run, err = self.runRepository.GetByNomadJobIdWithLock(id, lock)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q with lock %q", id, lock)
	return
}

func (self *runService) GetInputFactIdsByNomadJobId(id uuid.UUID) (inputFactIds repository.RunInputFactIds, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Msg("Getting Run input fact IDs by Nomad Job ID")
	inputFactIds, err = self.runRepository.GetInputFactIdsByNomadJobId(id)
	err = errors.WithMessagef(err, "Could not select Run input fact IDs by Nomad Job ID %q", id)
	return
}

func (self *runService) GetOutputByNomadJobId(id uuid.UUID) (output domain.RunOutput, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Msg("Getting Run Output by Nomad Job ID")
	output, err = self.runOutputRepository.GetByRunId(id)
	err = errors.WithMessagef(err, "Could not select existing Run Output by Nomad Job ID %q", id)
	return
}

func (self *runService) GetByActionId(id uuid.UUID, page *repository.Page) (runs []*domain.Run, err error) {
	self.logger.Trace().Str("id", id.String()).Int("offset", page.Offset).Int("limit", page.Limit).Msgf("Getting Run by Action ID")
	runs, err = self.runRepository.GetByActionId(id, page)
	err = errors.WithMessagef(err, "Could not select existing Run by Action ID %q with offset %d and limit %d", id, page.Offset, page.Limit)
	return
}

func (self *runService) GetLatestByActionId(id uuid.UUID) (run domain.Run, err error) {
	self.logger.Trace().Str("action-id", id.String()).Msg("Getting latest Run by Action ID")
	run, err = self.runRepository.GetLatestByActionId(id)
	err = errors.WithMessagef(err, "Could not select latest Run by Action ID %q", id)
	return
}

func (self *runService) GetAll(page *repository.Page) (runs []*domain.Run, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting all Runs")
	runs, err = self.runRepository.GetAll(page)
	err = errors.WithMessagef(err, "Could not select existing Runs with offset %d and limit %d", page.Offset, page.Limit)
	return
}

func (self *runService) GetByInputFactIds(factIds []*uuid.UUID, recursive bool, page *repository.Page) (runs []*domain.Run, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Interface("input-fact-ids", factIds).Bool("recursive", recursive).Msg("Getting Runs by input Fact IDs")
	runs, err = self.runRepository.GetByInputFactIds(factIds, recursive, page)
	err = errors.WithMessagef(err, "Could not select Runs by input fact IDs %q (recursively: %t) with offset %d and limit %d", factIds, recursive, page.Offset, page.Limit)
	return
}

func (self *runService) Save(run *domain.Run, inputs map[string]interface{}, output *domain.RunOutput) error {
	self.logger.Trace().Msg("Saving new Run")
	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.runRepository.WithQuerier(tx).Save(run, inputs); err != nil {
			return errors.WithMessagef(err, "Could not insert Run")
		}
		if err := self.runOutputRepository.WithQuerier(tx).Save(run.NomadJobID, output); err != nil {
			return errors.WithMessagef(err, "Could not insert Run Output")
		}
		return nil
	}); err != nil {
		return err
	}
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Created Run")
	return nil
}

func (self *runService) Update(run *domain.Run) error {
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Updating Run")
	if err := self.runRepository.Update(run); err != nil {
		return errors.WithMessagef(err, "Could not update Run with ID %q", run.NomadJobID)
	}
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Updated Run")
	return nil
}

func (self *runService) End(run *domain.Run) error {
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Ending Run")
	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.runRepository.WithQuerier(tx).Update(run); err != nil {
			return errors.WithMessagef(err, "Could not update Run with ID %q", run.NomadJobID)
		}
		if err := self.runOutputRepository.WithQuerier(tx).Delete(run.NomadJobID); err != nil {
			return errors.WithMessagef(err, "Could not delete Run Output with ID %q", run.NomadJobID)
		}
		if _, _, err := self.nomadClient.JobsDeregister(run.NomadJobID.String(), false, &nomad.WriteOptions{}); err != nil {
			return errors.WithMessagef(err, "Could not deregister Nomad job with ID %q", run.NomadJobID)
		}
		return nil
	}); err != nil {
		return err
	}
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Ended Run")
	return nil
}

func (self *runService) Cancel(run *domain.Run) error {
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Stopping Run")
	// Nomad does not know whether the job simply ran to finish
	// or was stopped manually. Delete output to avoid publishing it.
	if err := self.runOutputRepository.Delete(run.NomadJobID); err != nil {
		return err
	}
	if _, _, err := self.nomadClient.JobsDeregister(run.NomadJobID.String(), false, &nomad.WriteOptions{}); err != nil {
		return errors.WithMessagef(err, "Failed to deregister job %q", run.NomadJobID)
	}
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Stopped Run")
	return nil
}

func (self *runService) JobLogs(nomadJobID uuid.UUID, start time.Time, end *time.Time) (domain.LokiLog, error) {
	return self.LokiQueryRange(
		fmt.Sprintf(`{nomad_job_id=%q}`, nomadJobID.String()),
		start, end,
	)
}

func (self *runService) RunLogs(allocID, taskGroup, taskName string, start time.Time, end *time.Time) (domain.LokiLog, error) {
	return self.LokiQueryRange(
		fmt.Sprintf(`{nomad_alloc_id=%q,nomad_task_group=%q,nomad_task_name=%q}`, allocID, taskGroup, taskName),
		start, end,
	)
}

func (self *runService) LokiQueryRange(query string, start time.Time, end *time.Time) (domain.LokiLog, error) {
	linesToFetch := 10000
	// TODO: figure out the correct value for our infra, 5000 is the default configuration in loki
	var limit int64 = 5000
	output := domain.LokiLog{}

	if end == nil {
		now := time.Now().UTC()
		end = &now
	}

	endLater := end.Add(1 * time.Minute)
	end = &endLater

done:
	for {
		req, err := http.NewRequest(
			"GET",
			self.prometheus.URL("/loki/api/v1/query_range", nil).String(),
			http.NoBody,
		)
		if err != nil {
			return nil, err
		}

		q := req.URL.Query()
		q.Set("query", query)
		q.Set("limit", strconv.FormatInt(limit, 10))
		q.Set("start", strconv.FormatInt(start.UnixNano(), 10))
		q.Set("end", strconv.FormatInt(end.UnixNano(), 10))
		q.Set("direction", "FORWARD")
		req.URL.RawQuery = q.Encode()

		done, body, err := self.prometheus.Do(context.Background(), req)
		if err != nil {
			return nil, errors.WithMessage(err, "Failed to talk with loki")
		}

		if done.StatusCode/100 != 2 {
			return nil, fmt.Errorf("Error response %d from Loki: %s", done.StatusCode, string(body))
		}

		response := loghttp.QueryResponse{}

		err = json.Unmarshal(body, &response)
		if err != nil {
			return nil, err
		}

		streams, ok := response.Data.Result.(loghttp.Streams)
		if !ok {
			return nil, fmt.Errorf("Unexpected loki result type: %s", response.Data.Result.Type())
		}

		if len(streams) == 0 {
			break done
		}

		for _, stream := range streams {
			source, ok := stream.Labels.Map()["source"]
			if !ok {
				continue
			}

			for _, entry := range stream.Entries {
				output = append(output, domain.LokiLine{Time: entry.Timestamp, Text: entry.Line, Source: source})

				if (len(output)) >= linesToFetch {
					break done
				}
			}

			if int64(len(stream.Entries)) >= limit {
				start = stream.Entries[len(stream.Entries)-1].Timestamp
			} else if int64(len(stream.Entries)) < limit {
				break done
			}
		}
	}

	sort.Slice(output, func(i, j int) bool {
		return output[i].Time.Before(output[j].Time)
	})

	return output, nil
}
