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
)

func TestShouldGetRunByActionId(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	actionId := uuid.New()
	run := domain.Run{
		NomadJobID: uuid.New(),
		ActionId:   uuid.New(),
		CreatedAt:  now,
		FinishedAt: &now,
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"nomad_job_id", "action_id", "created_at", "finished_at"}).AddRow(run.NomadJobID, run.ActionId, run.CreatedAt, run.FinishedAt)
	mock.ExpectQuery("SELECT(.*)").WithArgs(actionId).WillReturnRows(rows)
	repository := NewRunRepository(mock)

	// when
	runResult, err := repository.GetByActionId(actionId)

	// then
	assert.Nil(t, err)
	assert.Equal(t, runResult[0].NomadJobID, run.NomadJobID)
	assert.Equal(t, runResult[0].ActionId, run.ActionId)
	assert.Equal(t, runResult[0].CreatedAt, run.CreatedAt)
	assert.Equal(t, runResult[0].FinishedAt, run.FinishedAt)
}

func TestShouldUpdateRun(t *testing.T) {
	t.Parallel()
	now := time.Now().UTC()
	run := domain.Run{
		NomadJobID: uuid.New(),
		ActionId:   uuid.New(),
		CreatedAt:  now,
		FinishedAt: &now,
	}

	// given
	mock, tx := mocks.BuildTransaction(context.Background(), t)
	mock.ExpectExec("UPDATE run").WithArgs(run.NomadJobID, run.FinishedAt).WillReturnResult(pgxmock.NewResult("UPDATE", 1))
	mock.ExpectCommit()
	repository := NewRunRepository(mock)

	// when
	err := repository.Update(tx, &run)

	// then
	assert.Nil(t, err)
}
