package persistence

import (
	"context"
	"encoding/json"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/stretchr/testify/assert"
	"testing"
	"time"

	"github.com/pashagolub/pgxmock"
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

	repository := NewActionRepository(mocks.NewDBMocked(mock))

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
	inputs := domain.InputDefinition{
		Select:   domain.InputDefinitionSelect(1),
		Not:      false,
		Optional: false,
		Match:    domain.InputDefinitionMatch(""),
	}
	actionInputs := make(map[string]domain.InputDefinition)
	actionInputs["inputs"] = inputs
	action := domain.Action{
		ID:     actionId,
		Name:   "Name",
		Source: "Source",
		ActionDefinition: domain.ActionDefinition{
			Meta:   map[string]interface{}{},
			Inputs: actionInputs,
		},
	}

	// given
	marshalInputs, err := json.Marshal(action.Inputs)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when marshaling the action.Inputs", err)
	}
	mock, tx := mocks.BuildTransaction(context.Background(), t)
	rows := mock.DB.NewRows([]string{"id", "created_at"}).AddRow(actionId, dateTime)
	mock.DB.ExpectQuery("INSERT INTO actions").WithArgs(action.ID, action.Name, action.Source, marshalInputs).WillReturnRows(rows)
	mock.DB.ExpectCommit()
	repository := NewActionRepository(mock)

	// when
	err = repository.Save(tx, &action)

	// then
	assert.Nil(t, err)
	assert.Equal(t, actionId, action.ID)
	assert.Equal(t, dateTime, action.CreatedAt)
}
