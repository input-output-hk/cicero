package persistence

import (
	"context"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/stretchr/testify/assert"
	"testing"
	"time"

	"github.com/pashagolub/pgxmock"
)

func TestShouldGetActionById(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	actionId, _ := uuid.NewUUID()
	action := domain.ActionInstance{
		ID:                 actionId,
		WorkflowInstanceId: uint64(1),
		Name:               "Name",
		Facts:              domain.Facts{},
		CreatedAt:          &now,
		UpdatedAt:          &now,
		FinishedAt:         &now,
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error %q was not expected when opening a stub database connection", err.Error())
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"id", "workflow_instance_id", "name", "facts",
		"created_at", "updated_at", "finished_at"}).AddRow(action.ID, action.WorkflowInstanceId, action.Name,
		action.Facts, action.CreatedAt, action.UpdatedAt, action.FinishedAt)
	mock.ExpectQuery("SELECT(.*)").WithArgs(actionId).WillReturnRows(rows)

	repository := NewActionRepository(mock)

	// when
	actionResult, err := repository.GetById(actionId)

	// then
	assert.Nil(t, err)
	assert.Equal(t, actionResult.ID, action.ID)
	assert.Equal(t, actionResult.WorkflowInstanceId, action.WorkflowInstanceId)
	assert.Equal(t, actionResult.Name, action.Name)
	assert.Equal(t, actionResult.Facts, action.Facts)
	assert.Equal(t, actionResult.CreatedAt, action.CreatedAt)
	assert.Equal(t, actionResult.UpdatedAt, action.UpdatedAt)
	assert.Equal(t, actionResult.FinishedAt, action.FinishedAt)
}

func TestShouldUpdateAction(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	actionId, _ := uuid.NewUUID()
	action := domain.ActionInstance{
		ID:                 actionId,
		WorkflowInstanceId: uint64(1),
		Name:               "Name",
		Facts:              domain.Facts{},
		CreatedAt:          &now,
		UpdatedAt:          &now,
		FinishedAt:         &now,
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error %q was not expected when opening a stub database connection", err.Error())
	}
	defer mock.Close(context.Background())
	mock.ExpectBegin()
	mock.ExpectExec("UPDATE action_instances").WithArgs(action.ID, action.FinishedAt, action.UpdatedAt, action.Facts).WillReturnResult(pgxmock.NewResult("UPDATE", 1))
	mock.ExpectCommit()
	repository := NewActionRepository(mock)

	ctx := context.Background()
	tx, err := mock.Begin(ctx)
	if err != nil {
		t.Fatalf("an error %q was not expected when Begin a Tx in database", err.Error())
	}
	defer func() { _ = tx.Rollback(ctx) }()

	// when
	err = repository.Update(tx, action)

	// then
	assert.Nil(t, err)
}
