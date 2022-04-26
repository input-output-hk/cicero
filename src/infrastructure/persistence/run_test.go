package persistence

import (
	"context"
	"testing"
	"time"

	"github.com/google/uuid"
	"github.com/pashagolub/pgxmock"
	"github.com/stretchr/testify/assert"

	"github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

func TestShouldGetRunByActionId(t *testing.T) {
	t.Skip("pgxmock does not support SendBatch()", "https://github.com/pashagolub/pgxmock/issues/52")

	t.Parallel()
	now := time.Now().UTC()
	actionId := uuid.New()

	run := domain.Run{
		NomadJobID:   uuid.New(),
		InvocationId: uuid.New(),
		CreatedAt:    now,
		FinishedAt:   &now,
	}

	page := repository.Page{
		Limit:  1,
		Offset: 0,
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error %q was not expected when opening a stub database connection", err)
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"nomad_job_id", "invocation_id", "created_at", "finished_at"}).AddRow(run.NomadJobID, run.InvocationId, run.CreatedAt, run.FinishedAt)
	mock.ExpectQuery("SELECT(.*)").WithArgs(actionId, page.Limit, page.Offset).WillReturnRows(rows)
	repository := NewRunRepository(mock)

	// when
	runResult, err := repository.GetByActionId(actionId, &page)

	// then
	assert.Nil(t, err)
	assert.Equal(t, runResult[0].NomadJobID, run.NomadJobID)
	assert.Equal(t, runResult[0].InvocationId, run.InvocationId)
	assert.Equal(t, runResult[0].CreatedAt, run.CreatedAt)
	assert.Equal(t, runResult[0].FinishedAt, run.FinishedAt)
}

func TestShouldUpdateRun(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	run := domain.Run{
		NomadJobID:   uuid.New(),
		InvocationId: uuid.New(),
		CreatedAt:    now,
		FinishedAt:   &now,
	}

	// given
	mock, _ := mocks.BuildTransaction(context.Background(), t)
	mock.ExpectExec("UPDATE run").WithArgs(run.NomadJobID, run.FinishedAt).WillReturnResult(pgxmock.NewResult("UPDATE", 1))
	mock.ExpectCommit()
	repository := NewRunRepository(mock)

	// when
	err := repository.Update(&run)

	// then
	assert.Nil(t, err)
}
