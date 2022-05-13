package service

import (
	"context"
	"encoding/json"
	"fmt"
	"html/template"
	"math"
	"net/http"
	"net/url"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/grafana/loki/pkg/loghttp"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/pborman/ansi"
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
	GetByInvocationId(uuid.UUID) (domain.Run, error)
	GetByActionId(uuid.UUID, *repository.Page) ([]*domain.Run, error)
	GetLatestByActionId(uuid.UUID) (domain.Run, error)
	GetAll(*repository.Page) ([]*domain.Run, error)
	Save(*domain.Run) error
	Update(*domain.Run) error
	End(*domain.Run) error
	Cancel(*domain.Run) error
	JobLogs(id uuid.UUID, start time.Time, end *time.Time) (domain.LokiLog, error)
	RunLogs(allocId, taskGroup, taskName string, start time.Time, end *time.Time) (domain.LokiLog, error)
	CPUMetrics(allocs map[string]domain.AllocationWithLogs, end *time.Time) (map[string][]*VMMetric, error)
	MemMetrics(allocs map[string]domain.AllocationWithLogs, end *time.Time) (map[string][]*VMMetric, error)
	GrafanaUrls(allocs map[string]domain.AllocationWithLogs, end *time.Time) (map[string]*url.URL, error)
}

type runService struct {
	logger              zerolog.Logger
	runRepository       repository.RunRepository
	prometheus          prometheus.Client
	victoriaMetricsAddr string
	nomadClient         application.NomadClient
	db                  config.PgxIface
}

func NewRunService(db config.PgxIface, prometheusAddr, victoriaMetricsAddr string, nomadClient application.NomadClient, logger *zerolog.Logger) RunService {
	impl := runService{
		logger:              logger.With().Str("component", "RunService").Logger(),
		runRepository:       persistence.NewRunRepository(db),
		nomadClient:         nomadClient,
		victoriaMetricsAddr: victoriaMetricsAddr,
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

func (self runService) WithQuerier(querier config.PgxIface) RunService {
	return &runService{
		logger:              self.logger,
		runRepository:       self.runRepository.WithQuerier(querier),
		prometheus:          self.prometheus,
		nomadClient:         self.nomadClient,
		db:                  querier,
	}
}

func (self runService) GetByNomadJobId(id uuid.UUID) (run domain.Run, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Msg("Getting Run by Nomad Job ID")
	run, err = self.runRepository.GetByNomadJobId(id)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q", id)
	return
}

func (self runService) GetByNomadJobIdWithLock(id uuid.UUID, lock string) (run domain.Run, err error) {
	self.logger.Trace().Str("nomad-job-id", id.String()).Str("lock", lock).Msg("Getting Run by Nomad Job ID with lock")
	run, err = self.runRepository.GetByNomadJobIdWithLock(id, lock)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q with lock %q", id, lock)
	return
}

func (self runService) GetByInvocationId(invocationId uuid.UUID) (run domain.Run, err error) {
	self.logger.Trace().Str("invocation-id", invocationId.String()).Msg("Getting Runs by input Fact IDs")
	run, err = self.runRepository.GetByInvocationId(invocationId)
	err = errors.WithMessagef(err, "Could not select Runs by Invocation ID %q", invocationId)
	return
}

func (self runService) GetByActionId(id uuid.UUID, page *repository.Page) (runs []*domain.Run, err error) {
	self.logger.Trace().Str("id", id.String()).Int("offset", page.Offset).Int("limit", page.Limit).Msgf("Getting Run by Action ID")
	runs, err = self.runRepository.GetByActionId(id, page)
	err = errors.WithMessagef(err, "Could not select existing Run by Action ID %q with offset %d and limit %d", id, page.Offset, page.Limit)
	return
}

func (self runService) GetLatestByActionId(id uuid.UUID) (run domain.Run, err error) {
	self.logger.Trace().Str("action-id", id.String()).Msg("Getting latest Run by Action ID")
	run, err = self.runRepository.GetLatestByActionId(id)
	err = errors.WithMessagef(err, "Could not select latest Run by Action ID %q", id)
	return
}

func (self runService) GetAll(page *repository.Page) (runs []*domain.Run, err error) {
	self.logger.Trace().Int("offset", page.Offset).Int("limit", page.Limit).Msg("Getting all Runs")
	runs, err = self.runRepository.GetAll(page)
	err = errors.WithMessagef(err, "Could not select existing Runs with offset %d and limit %d", page.Offset, page.Limit)
	return
}

func (self runService) Save(run *domain.Run) error {
	self.logger.Trace().Msg("Saving new Run")
	if err := self.runRepository.Save(run); err != nil {
		return errors.WithMessagef(err, "Could not insert Run")
	}
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Created Run")
	return nil
}

func (self runService) Update(run *domain.Run) error {
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Updating Run")
	if err := self.runRepository.Update(run); err != nil {
		return errors.WithMessagef(err, "Could not update Run with ID %q", run.NomadJobID)
	}
	self.logger.Trace().Str("id", run.NomadJobID.String()).Msg("Updated Run")
	return nil
}

func (self runService) End(run *domain.Run) error {
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Ending Run")
	if err := self.db.BeginFunc(context.Background(), func(tx pgx.Tx) error {
		if err := self.runRepository.WithQuerier(tx).Update(run); err != nil {
			return errors.WithMessagef(err, "Could not update Run with ID %q", run.NomadJobID)
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

func (self runService) Cancel(run *domain.Run) error {
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Stopping Run")
	// Nomad does not know whether the job simply ran to finish
	// or was stopped manually. Delete output to avoid publishing it.
	// FIXME we need some other way to tell Cicero not to publish the output fact
	// if err := self.runOutputRepository.Delete(run.NomadJobID); err != nil {
	// 	return err
	// }
	if _, _, err := self.nomadClient.JobsDeregister(run.NomadJobID.String(), false, &nomad.WriteOptions{}); err != nil {
		return errors.WithMessagef(err, "Failed to deregister job %q", run.NomadJobID)
	}
	self.logger.Debug().Str("id", run.NomadJobID.String()).Msg("Stopped Run")
	return nil
}

func (self runService) JobLogs(nomadJobID uuid.UUID, start time.Time, end *time.Time) (domain.LokiLog, error) {
	return self.LokiQueryRange(
		fmt.Sprintf(`{nomad_job_id=%q}`, nomadJobID.String()),
		start, end,
	)
}

func (self runService) RunLogs(allocID, taskGroup, taskName string, start time.Time, end *time.Time) (domain.LokiLog, error) {
	return self.LokiQueryRange(
		fmt.Sprintf(`{nomad_alloc_id=%q,nomad_task_group=%q,nomad_task_name=%q}`, allocID, taskGroup, taskName),
		start, end,
	)
}

func (self runService) LokiQueryRange(query string, start time.Time, end *time.Time) (domain.LokiLog, error) {
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

		entryCount := int64(0)
		for _, stream := range streams {
			source, ok := stream.Labels.Map()["source"]
			if !ok {
				continue
			}

			for _, entry := range stream.Entries {
				line := domain.LokiLine{Time: entry.Timestamp, Source: source, Text: entry.Line}
				lines := strings.Split(entry.Line, "\r")
				for _, l := range lines {
					if sane, err := ansi.Strip([]byte(l)); err == nil {
						line.Text = string(sane)
					} else {
						line.Text = l
					}
					output = append(output, line)
				}

				if (len(output)) >= linesToFetch {
					break done
				}
			}

			entryCount += int64(len(stream.Entries))
		}

		if entryCount >= limit {
			start = output[len(output)-1].Time
		} else {
			break done
		}
	}

	sort.Slice(output, func(i, j int) bool {
		return output[i].Time.Before(output[j].Time)
	})

	return output, nil
}

func (self runService) GrafanaUrls(allocs map[string]domain.AllocationWithLogs, to *time.Time) (map[string]*url.URL, error) {
	grafanaUrls := map[string]*url.URL{}

	for allocName, alloc := range allocs {
		from := time.UnixMicro(alloc.CreateTime / 1000) // there's no UnixNano
		if to == nil {
			t := time.UnixMicro(alloc.ModifyTime / 1000)
			to = &t
		}

		grafanaUrl, err := url.Parse("https://monitoring.infra.aws.iohkdev.io/d/SxGmPry7k/cgroups")
		if err != nil {
			return nil, err
		}
		guQuery := grafanaUrl.Query()

		guQuery.Set("orgId", "1")
		guQuery.Set("var-pattern", alloc.ID)
		guQuery.Set("from", strconv.FormatInt(from.UnixMilli(), 10))
		guQuery.Set("to", strconv.FormatInt(to.UnixMilli(), 10))
		grafanaUrl.RawQuery = guQuery.Encode()
		grafanaUrls[allocName] = grafanaUrl
	}
	return grafanaUrls, nil
}

func (self runService) metrics(allocs map[string]domain.AllocationWithLogs, to *time.Time, queryPattern string, labelFunc func(float64) template.HTML) (map[string][]*VMMetric, error) {
	vmUrl, err := url.Parse(self.victoriaMetricsAddr + "/api/v1/query_range")
	if err != nil {
		return nil, err
	}
	query := vmUrl.Query()

	metrics := map[string][]*VMMetric{}
	for allocName, alloc := range allocs {
		from := time.UnixMicro(alloc.CreateTime / 1000)
		if to == nil {
			t := time.UnixMicro(alloc.ModifyTime / 1000)
			to = &t
		}

		// calculate the appropriate step size, that is, every step gives you one point of data.
		// wtih too many steps, the graph becomes crowded
		// we target anything between 1 and 20 data points, if there is not a lot
		// of data, many steps will just display duplicated data because vector sends metrics every 10 seconds.
		// So the minimum size is 10
		// The maximum step size is duration/20
		duration := to.Sub(from).Seconds()

		step := math.Max(duration/20, 10)
		query.Set("step", strconv.FormatFloat(step, 'f', 0, 64))
		query.Set("start", strconv.FormatInt(from.Unix()-60, 10))
		query.Set("end", strconv.FormatInt(to.Unix()+60, 10))
		query.Set("query", fmt.Sprintf(queryPattern, alloc.ID))
		vmUrl.RawQuery = query.Encode()
		res, err := http.Get(vmUrl.String())
		if err != nil {
			return nil, err
		}

		metric := vmResponse{}
		if err := json.NewDecoder(res.Body).Decode(&metric); err != nil {
			return nil, err
		}
		if metric.Status != "success" {
			continue
		}

		max := float64(0)
		for _, r := range metric.Data.Result {
			for _, v := range r.Values {
				t := time.Unix(int64(v[0].(float64)), 0)
				f, err := strconv.ParseFloat(v[1].(string), 64)
				if err != nil {
					return nil, err
				}

				if f > max {
					max = f
				}

				vm := &VMMetric{Time: t, Size: f, Label: labelFunc(f)}
				metrics[allocName] = append(metrics[allocName], vm)
			}
		}

		for i, vm := range metrics[allocName] {
			vm.Size = ((100 / max) * vm.Size) / 100
			if i > 0 {
				vm.Start = metrics[allocName][i-1].Size
			} else {
				vm.Start = vm.Size
			}
		}
	}

	return metrics, nil
}

func (self runService) CPUMetrics(allocs map[string]domain.AllocationWithLogs, end *time.Time) (map[string][]*VMMetric, error) {
	return self.metrics(allocs, end, `rate(host_cgroup_cpu_usage_seconds_total{cgroup=~".*%s.*payload"})`, func(f float64) template.HTML {
		return template.HTML(strconv.FormatFloat(f, 'f', 1, 64))
	})
}

const (
	kib = 1024
	mib = kib * 1024
	gib = mib * 1024
	tib = gib * 1024
)

func (self runService) MemMetrics(allocs map[string]domain.AllocationWithLogs, end *time.Time) (map[string][]*VMMetric, error) {
	return self.metrics(allocs, end, `host_cgroup_memory_current_bytes{cgroup=~".*%s.*payload"}`, func(f float64) template.HTML {
		switch {
		case f >= tib:
			return template.HTML(strconv.FormatFloat(f/tib, 'f', 1, 64) + " TiB")
		case f >= gib:
			return template.HTML(strconv.FormatFloat(f/gib, 'f', 1, 64) + " GiB")
		case f >= mib:
			return template.HTML(strconv.FormatFloat(f/mib, 'f', 1, 64) + " MiB")
		case f >= kib:
			return template.HTML(strconv.FormatFloat(f/kib, 'f', 1, 64) + " KiB")
		default:
			return template.HTML(strconv.FormatFloat(f, 'f', 1, 64) + " B")
		}
	})
}

type vmResponse struct {
	Status string       `json:"status"`
	Data   vmResultData `json:"data"`
}

type vmResultData struct {
	ResultType string `json:"resultType"`
	Result     []vmResult
}

type vmResult struct {
	Metric map[string]string `json:"metric"`
	Values [][]interface{}   `json:"values"`
}

type VMMetric struct {
	Time  time.Time
	Start float64
	Size  float64
	Label template.HTML
}

// Groups metrics by timestamp.
// Useful to display as multiple datasets in one chart.
func GroupMetrics(seriesByAllocId ...map[string][]*VMMetric) map[string]map[time.Time][]*VMMetric {
	result := map[string]map[time.Time][]*VMMetric{}
	for seriesIdx, series := range seriesByAllocId {
		for allocId, seriesMetrics := range series {
			for _, seriesMetric := range seriesMetrics {
				// Make sure the map of groups for this allocation exists.
				if result[allocId] == nil {
					result[allocId] = map[time.Time][]*VMMetric{}
				}

				if group, exists := result[allocId][seriesMetric.Time]; exists {
					// A group for the same timestamp exists so add seriesMetric to it.
					group[seriesIdx] = seriesMetric
				} else {
					// No group for the same timestamp exists so create it.
					group = make([]*VMMetric, len(seriesByAllocId))
					group[seriesIdx] = seriesMetric
					result[allocId][seriesMetric.Time] = group
				}
			}
		}
	}
	return result
}
