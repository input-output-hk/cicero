package persistence

import (
	"context"
	"testing"
	"time"

	cueformat "cuelang.org/go/cue/format"
	"github.com/google/uuid"
	"github.com/pashagolub/pgxmock"
	"github.com/stretchr/testify/assert"

	"github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/input-output-hk/cicero/src/domain"
)

func TestShouldGetActionById(t *testing.T) {
	t.Parallel()
	actionId := uuid.New()
	action := domain.Action{
		ID:        actionId,
		Name:      "Name",
		Source:    "Source",
		CreatedAt: time.Now().UTC(),
	}

	// given
	mock, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer mock.Close(context.Background())
	rows := mock.NewRows([]string{"id", "name", "source", "created_at"}).AddRow(action.ID, action.Name,
		action.Source, action.CreatedAt)
	mock.ExpectQuery("SELECT(.*)").WithArgs(actionId).WillReturnRows(rows)

	repository := NewActionRepository(mock)

	// when
	ActionResult, err := repository.GetById(actionId)

	// then
	assert.Nil(t, err)
	assert.Equal(t, action.ID, ActionResult.ID)
	assert.Equal(t, action.Name, ActionResult.Name)
	assert.Equal(t, action.Source, ActionResult.Source)
	assert.Equal(t, action.CreatedAt, ActionResult.CreatedAt)
}

func TestShouldSaveAction(t *testing.T) {
	t.Parallel()
	dateTime := time.Now().UTC()
	actionId := uuid.New()
	action := domain.Action{
		ID:     actionId,
		Name:   "Name",
		Source: "Source",
		ActionDefinition: domain.ActionDefinition{
			Meta: map[string]interface{}{},
			InOut: domain.InOutDefinition{
				Inputs: domain.InputDefinitions{
					"a": domain.InputDefinition{
						Not:      false,
						Optional: false,
						Match:    domain.InOutCUEString(""),
					},
				},
			},
		},
	}

	// given
	ioStr, err := action.InOut.CUEString().Format(cueformat.Simplify(), cueformat.UseSpaces(0))
	if err != nil {
		t.Fatalf("an error %q was not expected when formatting the IO CUE", err)
	}
	mock, _ := mocks.BuildTransaction(context.Background(), t)
	rows := mock.NewRows([]string{"id", "created_at"}).AddRow(actionId, dateTime)
	mock.ExpectQuery("INSERT INTO action").WithArgs(action.ID, action.Name, action.Source, ioStr).WillReturnRows(rows)
	mock.ExpectCommit()
	repository := NewActionRepository(mock)

	// when
	err = repository.Save(&action)

	// then
	assert.Nil(t, err)
	assert.Equal(t, actionId, action.ID)
	assert.Equal(t, dateTime, action.CreatedAt)
}
