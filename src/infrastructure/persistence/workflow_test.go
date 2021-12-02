package persistence

import (
	"context"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/stretchr/testify/assert"
	"testing"
	"time"

	"github.com/pashagolub/pgxmock"
)

func TestShouldGetWorkflowById(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	workflowId := uint64(1)

	workflow := domain.WorkflowInstance{
		ID:        workflowId,
		Name:      "Name",
		Source:    "Source",
		Facts:     domain.Facts{},
		CreatedAt: &now,
		UpdatedAt: &now,
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"id", "name", "source",
		"facts", "created_at", "updated_at"}).AddRow(workflow.ID, workflow.Name,
		workflow.Source, workflow.Facts, workflow.CreatedAt, workflow.UpdatedAt)
	mock.ExpectQuery("SELECT(.*)").WithArgs(workflowId).WillReturnRows(rows)

	repository := NewWorkflowRepository(mock)

	// when
	workflowResult, err := repository.GetById(workflowId)

	// then
	assert.Nil(t, err)
	assert.Equal(t, workflowResult.ID, workflow.ID)
	assert.Equal(t, workflowResult.Name, workflow.Name)
	assert.Equal(t, workflowResult.Source, workflow.Source)
	assert.Equal(t, workflowResult.Facts, workflow.Facts)
	assert.Equal(t, workflowResult.CreatedAt, workflow.CreatedAt)
	assert.Equal(t, workflowResult.UpdatedAt, workflow.UpdatedAt)
}

func TestShouldSaveWorkflow(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	workflowId := uint64(1)
	workflow := domain.WorkflowInstance{
		Name:   "Name",
		Source: "Source",
		Facts:  domain.Facts{},
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"id", "created_at"}).AddRow(workflowId, &now)
	mock.ExpectBegin()
	mock.ExpectQuery("INSERT INTO workflow_instances").WithArgs(workflow.Source, workflow.Name, workflow.Facts).WillReturnRows(rows)
	mock.ExpectCommit()
	repository := NewWorkflowRepository(mock)

	ctx := context.Background()
	tx, err := mock.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	defer tx.Rollback(ctx)

	// when
	err = repository.Save(tx, &workflow)

	// then
	assert.Nil(t, err)
	assert.Equal(t, workflowId, workflow.ID)
	assert.Equal(t, now, *workflow.CreatedAt)
}
