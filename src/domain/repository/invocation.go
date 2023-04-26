package repository

import (
	"encoding/json"
	"net/url"

	"github.com/google/uuid"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/util"
)

type InvocationRepository interface {
	WithQuerier(config.PgxIface) InvocationRepository

	GetById(uuid.UUID) (*domain.Invocation, error)
	GetLatestByActionId(uuid.UUID) (*domain.Invocation, error)
	GetInputFactIdsById(uuid.UUID) (map[string]uuid.UUID, error)
	Get(*Page, InvocationGetOpts) ([]domain.Invocation, error)
	GetByInputFactIds([]*uuid.UUID, bool, util.MayBool, *Page) ([]domain.Invocation, error)
	Save(*domain.Invocation, map[string]domain.Fact) error
	End(uuid.UUID) error
}

type InvocationFilter struct {
	InvocationInvoking util.MayBool
	InvocationFailed   util.MayBool
	RunRunning         util.MayBool
	RunSucceeded       util.MayBool
	RunFailed          util.MayBool
	RunCanceled        util.MayBool
}

func (self InvocationFilter) String() string {
	if j, err := json.Marshal(self); err != nil {
		return err.Error()
	} else {
		return string(j)
	}
}

func (self InvocationFilter) URLSearchParams() url.Values {
	search := make(url.Values, 6)
	add := func(name string, v util.MayBool) {
		if p := v.Ptr(); p != nil {
			var value string
			if *p {
				value = "true"
			}
			search.Add(name, value)
		}
	}

	add("filter_invocation_invoking", self.InvocationInvoking)
	add("filter_invocation_failed", self.InvocationFailed)
	add("filter_run_running", self.RunRunning)
	add("filter_run_succeeded", self.RunSucceeded)
	add("filter_run_failed", self.RunFailed)
	add("filter_run_canceled", self.RunCanceled)

	return search
}

func NewInvocationFilter[T url.Values](values T) InvocationFilter {
	var self InvocationFilter

	switch t := any(values).(type) {
	case url.Values:
		if v, has := t["filter_invocation_invoking"]; has {
			self.InvocationInvoking = util.NewMayBool(v[0] != "")
		}
		if v, has := t["filter_invocation_failed"]; has {
			self.InvocationFailed = util.NewMayBool(v[0] != "")
		}
		if v, has := t["filter_run_running"]; has {
			self.RunRunning = util.NewMayBool(v[0] != "")
		}
		if v, has := t["filter_run_succeeded"]; has {
			self.RunSucceeded = util.NewMayBool(v[0] != "")
		}
		if v, has := t["filter_run_failed"]; has {
			self.RunFailed = util.NewMayBool(v[0] != "")
		}
		if v, has := t["filter_run_canceled"]; has {
			self.RunCanceled = util.NewMayBool(v[0] != "")
		}
	default:
		panic("Unknown type. This is a bug.")
	}

	return self
}

type InvocationGetOpts struct {
	Private  util.MayBool
	Filter   InvocationFilter
	ActionId *uuid.UUID
}
