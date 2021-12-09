// Code generated by mockery v2.9.4. DO NOT EDIT.

package mocks

import (
	domain "github.com/input-output-hk/cicero/src/domain"
	mock "github.com/stretchr/testify/mock"

	pgx "github.com/jackc/pgx/v4"

	uuid "github.com/google/uuid"
)

// ActionService is an autogenerated mock type for the ActionService type
type ActionService struct {
	mock.Mock
}

// ActionLogs provides a mock function with given fields: allocId, taskGroup
func (_m *ActionService) ActionLogs(allocId string, taskGroup string) (*domain.LokiOutput, error) {
	ret := _m.Called(allocId, taskGroup)

	var r0 *domain.LokiOutput
	if rf, ok := ret.Get(0).(func(string, string) *domain.LokiOutput); ok {
		r0 = rf(allocId, taskGroup)
	} else {
		if ret.Get(0) != nil {
			r0 = ret.Get(0).(*domain.LokiOutput)
		}
	}

	var r1 error
	if rf, ok := ret.Get(1).(func(string, string) error); ok {
		r1 = rf(allocId, taskGroup)
	} else {
		r1 = ret.Error(1)
	}

	return r0, r1
}

// GetAll provides a mock function with given fields:
func (_m *ActionService) GetAll() ([]*domain.ActionInstance, error) {
	ret := _m.Called()

	var r0 []*domain.ActionInstance
	if rf, ok := ret.Get(0).(func() []*domain.ActionInstance); ok {
		r0 = rf()
	} else {
		if ret.Get(0) != nil {
			r0 = ret.Get(0).([]*domain.ActionInstance)
		}
	}

	var r1 error
	if rf, ok := ret.Get(1).(func() error); ok {
		r1 = rf()
	} else {
		r1 = ret.Error(1)
	}

	return r0, r1
}

// GetById provides a mock function with given fields: _a0
func (_m *ActionService) GetById(_a0 uuid.UUID) (domain.ActionInstance, error) {
	ret := _m.Called(_a0)

	var r0 domain.ActionInstance
	if rf, ok := ret.Get(0).(func(uuid.UUID) domain.ActionInstance); ok {
		r0 = rf(_a0)
	} else {
		r0 = ret.Get(0).(domain.ActionInstance)
	}

	var r1 error
	if rf, ok := ret.Get(1).(func(uuid.UUID) error); ok {
		r1 = rf(_a0)
	} else {
		r1 = ret.Error(1)
	}

	return r0, r1
}

// GetByNameAndWorkflowId provides a mock function with given fields: name, workflowId
func (_m *ActionService) GetByNameAndWorkflowId(name string, workflowId uint64) (domain.ActionInstance, error) {
	ret := _m.Called(name, workflowId)

	var r0 domain.ActionInstance
	if rf, ok := ret.Get(0).(func(string, uint64) domain.ActionInstance); ok {
		r0 = rf(name, workflowId)
	} else {
		r0 = ret.Get(0).(domain.ActionInstance)
	}

	var r1 error
	if rf, ok := ret.Get(1).(func(string, uint64) error); ok {
		r1 = rf(name, workflowId)
	} else {
		r1 = ret.Error(1)
	}

	return r0, r1
}

// JobLogs provides a mock function with given fields: _a0
func (_m *ActionService) JobLogs(_a0 uuid.UUID) (*domain.LokiOutput, error) {
	ret := _m.Called(_a0)

	var r0 *domain.LokiOutput
	if rf, ok := ret.Get(0).(func(uuid.UUID) *domain.LokiOutput); ok {
		r0 = rf(_a0)
	} else {
		if ret.Get(0) != nil {
			r0 = ret.Get(0).(*domain.LokiOutput)
		}
	}

	var r1 error
	if rf, ok := ret.Get(1).(func(uuid.UUID) error); ok {
		r1 = rf(_a0)
	} else {
		r1 = ret.Error(1)
	}

	return r0, r1
}

// Save provides a mock function with given fields: _a0, _a1
func (_m *ActionService) Save(_a0 pgx.Tx, _a1 *domain.ActionInstance) error {
	ret := _m.Called(_a0, _a1)

	var r0 error
	if rf, ok := ret.Get(0).(func(pgx.Tx, *domain.ActionInstance) error); ok {
		r0 = rf(_a0, _a1)
	} else {
		r0 = ret.Error(0)
	}

	return r0
}

// Update provides a mock function with given fields: _a0, _a1
func (_m *ActionService) Update(_a0 pgx.Tx, _a1 domain.ActionInstance) error {
	ret := _m.Called(_a0, _a1)

	var r0 error
	if rf, ok := ret.Get(0).(func(pgx.Tx, domain.ActionInstance) error); ok {
		r0 = rf(_a0, _a1)
	} else {
		r0 = ret.Error(0)
	}

	return r0
}
