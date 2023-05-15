package service

import (
	"context"
	"encoding/json"
	"fmt"
	"html/template"
	"math"
	"net/http"
	"net/url"
	"strconv"
	"time"

	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	nomadStructs "github.com/hashicorp/nomad/nomad/structs"
	"github.com/jackc/pgx/v5"
	"github.com/madflojo/tasks"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"

	"github.com/input-output-hk/cicero/src/application"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type RunService interface {
	WithQuerier(config.PgxIface) RunService

	InitTimeouts() error
	GetByNomadJobId(uuid.UUID) (*domain.Run, error)
	GetByNomadJobIdWithLock(uuid.UUID, string) (*domain.Run, error)
	GetByInvocationId(uuid.UUID) (*domain.Run, error)
	GetLatestByActionId(uuid.UUID) (*domain.Run, error)
	Get(*repository.Page, repository.RunGetOpts) ([]domain.Run, error)
	Save(*domain.Run, config.PgxIface) error
	Update(*domain.Run) error
	End(*domain.Run) error
	Cancel(*domain.Run) error
	JobLog(context.Context, domain.Run) LokiLineChan
	TaskLog(context.Context, nomad.Allocation, string) LokiLineChan
	GetRunAllocations(domain.Run) (Allocations, error)
	CPUMetrics(allocs []nomad.Allocation, end *time.Time) (map[string][]VMMetric, error)
	MemMetrics(allocs []nomad.Allocation, end *time.Time) (map[string][]VMMetric, error)
	GrafanaUrls(allocs []nomad.Allocation, end *time.Time) (map[string]*url.URL, error)
	GrafanaLokiUrls(allocs []nomad.Allocation, end *time.Time) (map[string]*url.URL, error)
}

type runService struct {
	logger              zerolog.Logger
	runRepository       repository.RunRepository
	lokiService         LokiService
	victoriaMetricsAddr string
	nomadEventService   NomadEventService
	nomadClient         application.NomadClient
	runTimeout          time.Duration
	timeouts            *tasks.Scheduler
	db                  config.PgxIface
}

func NewRunService(db config.PgxIface, lokiService LokiService, nomadEventService NomadEventService, victoriaMetricsAddr string, nomadClient application.NomadClient, runTimeout time.Duration, logger *zerolog.Logger) RunService {
	return &runService{
		logger:              logger.With().Str("component", "RunService").Logger(),
		runRepository:       persistence.NewRunRepository(db),
		nomadClient:         nomadClient,
		nomadEventService:   nomadEventService,
		lokiService:         lokiService,
		victoriaMetricsAddr: victoriaMetricsAddr,
		runTimeout:          runTimeout,
		timeouts:            tasks.New(),
		db:                  db,
	}
}

func (self runService) WithQuerier(querier config.PgxIface) RunService {
	return &runService{
		logger:            self.logger,
		runRepository:     self.runRepository.WithQuerier(querier),
		nomadEventService: self.nomadEventService.WithQuerier(querier),
		lokiService:       self.lokiService,
		nomadClient:       self.nomadClient,
		runTimeout:        self.runTimeout,
		timeouts:          self.timeouts,
		db:                querier,
	}
}

func (self runService) initTimeout(run domain.Run, db config.PgxIface) error {
	if run.FinishedAt != nil {
		return nil
	}

	timeout := run.CreatedAt.Add(self.runTimeout)
	self.logger.Trace().Stringer("id", run.NomadJobID).Stringer("timeout", timeout).Msg("Initializing timeout")
	return errors.WithMessage(self.timeouts.AddWithID(run.NomadJobID.String(), &tasks.Task{
		RunOnce:    true,
		StartAfter: timeout,
		Interval:   1,
		TaskFunc: func() error {
			self.logger.Info().Stringer("id", run.NomadJobID).Msg("Canceling due to timeout")
			return self.WithQuerier(db).Cancel(&run)
		},
		ErrFunc: func(err error) {
			self.logger.Err(err).Stringer("id", run.NomadJobID).Msg("Error canceling Run after timeout")
		},
	}), "While scheduling timeout")
}

func (self runService) InitTimeouts() error {
	runs, err := self.Get(nil, repository.RunGetOpts{Status: []domain.RunStatus{domain.RunStatusRunning}})
	if err != nil {
		return err
	}

	for _, run := range runs {
		if err := self.initTimeout(run, self.db); err != nil {
			return err
		}
	}

	return nil
}

func (self runService) GetByNomadJobId(id uuid.UUID) (run *domain.Run, err error) {
	self.logger.Trace().Stringer("nomad-job-id", id).Msg("Getting Run by Nomad Job ID")
	run, err = self.runRepository.GetByNomadJobId(id)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q", id)
	return
}

func (self runService) GetByNomadJobIdWithLock(id uuid.UUID, lock string) (run *domain.Run, err error) {
	self.logger.Trace().Stringer("nomad-job-id", id).Str("lock", lock).Msg("Getting Run by Nomad Job ID with lock")
	run, err = self.runRepository.GetByNomadJobIdWithLock(id, lock)
	err = errors.WithMessagef(err, "Could not select existing Run by Nomad Job ID %q with lock %q", id, lock)
	return
}

func (self runService) GetByInvocationId(invocationId uuid.UUID) (run *domain.Run, err error) {
	self.logger.Trace().Stringer("invocation-id", invocationId).Msg("Getting Run by Invocation ID")
	run, err = self.runRepository.GetByInvocationId(invocationId)
	err = errors.WithMessagef(err, "Could not select Run by Invocation ID %q", invocationId)
	return
}

func (self runService) GetLatestByActionId(id uuid.UUID) (run *domain.Run, err error) {
	self.logger.Trace().Stringer("action-id", id).Msg("Getting latest Run by Action ID")
	run, err = self.runRepository.GetLatestByActionId(id)
	err = errors.WithMessagef(err, "Could not select latest Run by Action ID %q", id)
	return
}

func (self runService) Get(page *repository.Page, opts repository.RunGetOpts) (runs []domain.Run, err error) {
	optsJson, _ := json.Marshal(opts)
	pageJson, _ := json.Marshal(page)
	self.logger.Trace().RawJSON("page", pageJson).RawJSON("opts", optsJson).Msg("Getting Runs")
	runs, err = self.runRepository.Get(page, opts)
	err = errors.WithMessagef(err, "Could not select existing Runs with page %s", pageJson)
	return
}

func (self runService) Save(run *domain.Run, db config.PgxIface) error {
	self.logger.Trace().Msg("Saving new Run")
	if err := self.runRepository.Save(run); err != nil {
		return errors.WithMessage(err, "Could not insert Run")
	}
	return self.initTimeout(*run, db)
}

func (self runService) Update(run *domain.Run) error {
	self.logger.Trace().Stringer("id", run.NomadJobID).Msg("Updating Run")
	if err := self.runRepository.Update(run); err != nil {
		return errors.WithMessagef(err, "Could not update Run with ID %q", run.NomadJobID)
	}
	self.logger.Trace().Stringer("id", run.NomadJobID).Msg("Updated Run")
	return nil
}

func (self runService) End(run *domain.Run) error {
	self.logger.Debug().Stringer("id", run.NomadJobID).Msg("Ending Run")
	if err := pgx.BeginFunc(context.Background(), self.db, func(tx pgx.Tx) error {
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
	self.timeouts.Del(run.NomadJobID.String())
	self.logger.Debug().Stringer("id", run.NomadJobID).Msg("Ended Run")
	return nil
}

func (self runService) Cancel(run *domain.Run) error {
	self.logger.Debug().Stringer("id", run.NomadJobID).Msg("Stopping Run")
	run.Status = domain.RunStatusCanceled
	if err := self.Update(run); err != nil {
		return err
	}
	if _, _, err := self.nomadClient.JobsDeregister(run.NomadJobID.String(), false, &nomad.WriteOptions{}); err != nil {
		return errors.WithMessagef(err, "Failed to deregister job %q", run.NomadJobID)
	}
	self.timeouts.Del(run.NomadJobID.String())
	self.logger.Debug().Stringer("id", run.NomadJobID).Msg("Stopped Run")
	return nil
}

func (self runService) JobLog(ctx context.Context, run domain.Run) LokiLineChan {
	return self.lokiService.QueryTailRange(
		ctx,
		fmt.Sprintf(`{nomad_job_id=%q}`, run.NomadJobID.String()),
		run.CreatedAt, run.FinishedAt,
	)
}

func (self runService) TaskLog(ctx context.Context, alloc nomad.Allocation, task string) LokiLineChan {
	taskState := alloc.TaskStates[task]

	var finishedAt *time.Time
	if taskState.State == nomadStructs.TaskStateDead {
		finishedAt = &taskState.FinishedAt

		// tolerate logs shipped up to one minute late
		late := finishedAt.Add(time.Minute)
		// must not be in the future or `QueryTailRange()` will panic
		now := time.Now()
		if late.After(now) {
			late = now
		}
		finishedAt = &late
	}

	return self.lokiService.QueryTailRange(
		ctx,
		fmt.Sprintf(`{nomad_alloc_id=%q,nomad_task_name=%q}`, alloc.ID, task),
		taskState.StartedAt, finishedAt,
	)
}

func (self runService) GetRunAllocations(run domain.Run) (Allocations, error) {
	return self.nomadEventService.GetLatestEventAllocationByJobId(run.NomadJobID)
}

type Allocations []nomad.Allocation

func (self Allocations) ByGroup() map[string][]nomad.Allocation {
	byGroup := map[string][]nomad.Allocation{}
	for _, alloc := range self {
		byGroup[alloc.TaskGroup] = append(byGroup[alloc.TaskGroup], alloc)
	}
	return byGroup
}

func (self runService) GrafanaUrls(allocs []nomad.Allocation, to *time.Time) (map[string]*url.URL, error) {
	grafanaUrls := map[string]*url.URL{}

	for _, alloc := range allocs {
		from := time.UnixMicro(alloc.CreateTime / 1000) // there's no UnixNano
		if to == nil {
			t := time.UnixMicro(alloc.ModifyTime / 1000)
			to = &t
		}

		grafanaUrl, err := url.Parse("https://monitoring.ci.iog.io/d/SxGmPry7k/cgroups")
		if err != nil {
			return nil, err
		}
		guQuery := grafanaUrl.Query()

		guQuery.Set("orgId", "1")
		guQuery.Set("var-pattern", alloc.ID)
		guQuery.Set("from", strconv.FormatInt(from.UnixMilli(), 10))
		guQuery.Set("to", strconv.FormatInt(to.UnixMilli(), 10))
		grafanaUrl.RawQuery = guQuery.Encode()
		grafanaUrls[alloc.ID] = grafanaUrl
	}
	return grafanaUrls, nil
}

func (self runService) GrafanaLokiUrls(allocs []nomad.Allocation, to *time.Time) (map[string]*url.URL, error) {
	grafanaUrls := map[string]*url.URL{}

	for _, alloc := range allocs {
		from := time.UnixMicro(alloc.CreateTime / 1000) // there's no UnixNano
		if to == nil && alloc.ClientTerminalStatus() {
			t := time.UnixMicro(alloc.ModifyTime / 1000)
			to = &t
		}
		var toStr string
		if to == nil {
			toStr = "now"
		} else {
			toStr = strconv.FormatInt(to.UnixMilli(), 10)
		}

		grafanaUrl, err := url.Parse("https://monitoring.ci.iog.io/explore")
		if err != nil {
			return nil, err
		}
		guQuery := grafanaUrl.Query()

		left, err := json.Marshal(
			grafanaExplore{
				Datasource: "Loki",
				Queries: []grafanaExploreQuery{
					{
						RefId:      "A",
						EditorMode: "builder",
						Expr:       fmt.Sprintf("{nomad_alloc_id=\"%s\"} |= ``", alloc.ID),
						QueryType:  "range",
					},
				},
				Range: grafanaExploreRange{
					From: strconv.FormatInt(from.UnixMilli(), 10),
					To:   toStr,
				},
			},
		)
		if err != nil {
			return nil, err
		}

		guQuery.Set("orgId", "1")
		guQuery.Set("left", string(left))
		grafanaUrl.RawQuery = guQuery.Encode()
		grafanaUrls[alloc.ID] = grafanaUrl
	}

	return grafanaUrls, nil
}

type grafanaExplore struct {
	Datasource string                `json:"datasource"`
	Queries    []grafanaExploreQuery `json:"queries"`
	Range      grafanaExploreRange   `json:"range"`
}

type grafanaExploreQuery struct {
	RefId      string `json:"refId"`
	EditorMode string `json:"editorMode"`
	Expr       string `json:"expr"`
	QueryType  string `json:"queryType"`
}

type grafanaExploreRange struct {
	From string `json:"from"`
	To   string `json:"to"`
}

func (self runService) metrics(allocs []nomad.Allocation, to *time.Time, queryPattern string, labelFunc func(float64) template.HTML) (map[string][]VMMetric, error) {
	vmUrl, err := url.Parse(self.victoriaMetricsAddr + "/api/v1/query_range")
	if err != nil {
		return nil, err
	}
	query := vmUrl.Query()

	metrics := map[string][]VMMetric{}
	for _, alloc := range allocs {
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

				metrics[alloc.ID] = append(
					metrics[alloc.ID],
					VMMetric{Time: t, Size: f, Label: labelFunc(f)},
				)
			}
		}

		for i, vm := range metrics[alloc.ID] {
			vm.Size = ((100 / max) * vm.Size) / 100
			if i > 0 {
				vm.Start = metrics[alloc.ID][i-1].Size
			} else {
				vm.Start = vm.Size
			}
		}
	}

	return metrics, nil
}

func (self runService) CPUMetrics(allocs []nomad.Allocation, end *time.Time) (map[string][]VMMetric, error) {
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

func (self runService) MemMetrics(allocs []nomad.Allocation, end *time.Time) (map[string][]VMMetric, error) {
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
	Values [][]any           `json:"values"`
}

type VMMetric struct {
	Time  time.Time
	Start float64
	Size  float64
	Label template.HTML
}

// Groups metrics by timestamp.
// Useful to display as multiple datasets in one chart.
func GroupMetrics(seriesByAllocId ...map[string][]VMMetric) map[string]map[time.Time][]VMMetric {
	result := map[string]map[time.Time][]VMMetric{}
	for seriesIdx, series := range seriesByAllocId {
		for allocId, seriesMetrics := range series {
			for _, seriesMetric := range seriesMetrics {
				// Make sure the map of groups for this allocation exists.
				if result[allocId] == nil {
					result[allocId] = map[time.Time][]VMMetric{}
				}

				if group, exists := result[allocId][seriesMetric.Time]; exists {
					// A group for the same timestamp exists so add seriesMetric to it.
					group[seriesIdx] = seriesMetric
				} else {
					// No group for the same timestamp exists so create it.
					group = make([]VMMetric, len(seriesByAllocId))
					group[seriesIdx] = seriesMetric
					result[allocId][seriesMetric.Time] = group
				}
			}
		}
	}
	return result
}
