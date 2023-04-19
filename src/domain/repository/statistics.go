package repository

import (
	"encoding/json"

	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/util"
)

type StatisticsRepository interface {
	WithQuerier(config.PgxIface) StatisticsRepository

	ActionStatus([]uuid.UUID, []string, util.MayBool) (ActionsStatusStatistics, error)
}

type ActionsStatusStatistics []IdentifiedActionStatusStatistics

type IdentifiedActionStatusStatistics struct {
	Id   uuid.UUID `json:"id"`
	Name string    `json:"name"`
	ActionStatusStatistics
}

func (self ActionsStatusStatistics) Total() ActionStatusStatistics {
	total := ActionStatusStatistics{}
	for _, v := range self {
		total = total.Add(v.ActionStatusStatistics)
	}
	return total
}

func (self ActionsStatusStatistics) ById() map[uuid.UUID]ActionStatusStatistics {
	byId := map[uuid.UUID]ActionStatusStatistics{}
	for _, v := range self {
		byId[v.Id] = v.ActionStatusStatistics
	}
	return byId
}

func (self ActionsStatusStatistics) ByName() map[string]ActionStatusStatistics {
	byName := map[string]ActionStatusStatistics{}
	for _, v := range self {
		byName[v.Name] = byName[v.Name].Add(v.ActionStatusStatistics)
	}
	return byName
}

type ActionStatusStatistics struct {
	RunsRunning         uint `json:"runs_running"`
	RunsFailed          uint `json:"runs_failed"`
	RunsSucceeded       uint `json:"runs_succeeded"`
	RunsCanceled        uint `json:"runs_canceled"`
	Invocations         uint `json:"invocations"`
	InvocationsFinished uint `json:"invocations_finished"`
}

func NewActionStatusStatistic(runsRunning, runsFailed, runsSucceeded, runsCanceled, invocations, invocationsFinished uint) ActionStatusStatistics {
	return ActionStatusStatistics{runsRunning, runsFailed, runsSucceeded, runsCanceled, invocations, invocationsFinished}
}

func (self ActionStatusStatistics) Add(stats ...ActionStatusStatistics) ActionStatusStatistics {
	total := self
	for _, v := range stats {
		total.RunsRunning += v.RunsRunning
		total.RunsFailed += v.RunsFailed
		total.RunsSucceeded += v.RunsSucceeded
		total.RunsCanceled += v.RunsCanceled
		total.Invocations += v.Invocations
		total.InvocationsFinished += v.InvocationsFinished
	}
	return total
}

func (self ActionStatusStatistics) Runs() uint {
	return self.RunsRunning + self.RunsFailed + self.RunsSucceeded + self.RunsCanceled
}

func (self ActionStatusStatistics) RunsFinished() uint {
	return self.Runs() - self.RunsRunning
}

func (self ActionStatusStatistics) InvocationsInvoking() uint {
	return self.Invocations - self.InvocationsFinished
}

func (self ActionStatusStatistics) InvocationsSucceeded() uint {
	return self.Runs()
}

func (self ActionStatusStatistics) InvocationsFailed() uint {
	return self.InvocationsFinished - self.InvocationsSucceeded()
}

func (self ActionStatusStatistics) MarshalJSON() ([]byte, error) {
	return json.Marshal(map[string]map[string]uint{
		"runs": {
			"total":     self.Runs(),
			"finished":  self.RunsFinished(),
			"running":   self.RunsRunning,
			"failed":    self.RunsFailed,
			"succeeded": self.RunsSucceeded,
			"canceled":  self.RunsCanceled,
		},
		"invocations": {
			"total":     self.Invocations,
			"finished":  self.InvocationsFinished,
			"invoking":  self.InvocationsInvoking(),
			"succeeded": self.InvocationsSucceeded(),
			"failed":    self.InvocationsFailed(),
		},
	})
}
