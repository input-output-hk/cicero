package component

import (
	"context"
	"errors"
	"fmt"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application/mocks"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/pashagolub/pgxmock"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"log"
	"os"
	"testing"
)

func buildNomadEventConsumerMocked(nomadEventServiceMocked *mocks.NomadEventService,
	nomadClientMocked *mocks.NomadClient, db config.PgxIface) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:            log.New(os.Stderr, "NomadEventConsumerTest: ", log.LstdFlags),
		NomadEventService: nomadEventServiceMocked,
		NomadClient:       nomadClientMocked,
		Db:                db,
	}
}

/*func generateEvents(events []nomad.Events) <-chan *nomad.Events {
	rc := make(chan *nomad.Events, len(events))
	go func() {
		defer close(rc)
		for _, event := range events {
			rc <- &event
		}
	}()
	return rc
}*/

func generateEvents(ctx context.Context, events []nomad.Events) <-chan *nomad.Events {
	dst := make(chan *nomad.Events)
	go func() {
		defer close(dst)
		for _, event := range events {
			dst <- &event
		}
		<-ctx.Done()
		return
	}()
	return dst
}

func TestStartWorkflowFailureToListenNomadEvent(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	stream := generateEvents(ctx, []nomad.Events{})
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	errorMessage := "Some error"
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, errors.New(errorMessage))

	//when
	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not listen to Nomad events: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureGettingNextEvents(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	events := []nomad.Events{
		{Err: errors.New(errorMessage)},
	}
	stream := generateEvents(ctx, events)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	//when
	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), fmt.Errorf("Error getting next events from Nomad event stream: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

/*func TestStartWorkflowGettingEventAllocationFailure(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation"}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(events)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	//when
	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), "Error getting Nomad event's allocation")
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}*/

func mockConnection(t *testing.T) pgxmock.PgxConnIface {
	t.Helper()

	conn, err := pgxmock.NewConn(pgxmock.QueryMatcherOption(pgxmock.QueryMatcherEqual))
	assert.NoError(t, err)

	conn.MatchExpectationsInOrder(true)

	return conn
}

func TestStartWorkflowFailureToSaveEvent(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation"}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(ctx, events)

	dbMocked := mockConnection(t)
	defer dbMocked.Close(context.Background())
	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()
	dbMocked.ExpectRollback()
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, dbMocked)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)
	nomadEventService.On("Save", mock.AnythingOfType("*pgxmock.pgxmock"), mock.AnythingOfType("*api.Event")).Return(errors.New(errorMessage))

	//when
	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), fmt.Errorf("Error Saving the Nomad event: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureToSaveEvent2(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation"}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(ctx, events)
	/*dbMocked, err := pgxmock.NewConn()
	if err != nil {
		t.Fatalf("an error '%s' was not expected when opening a stub database connection", err)
	}
	defer dbMocked.Close(ctx)
	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()
	tx, err := dbMocked.Begin(ctx)
	if err != nil {
		t.Fatalf("an error '%s' was not expected when Begin a Tx in database", err)
	}
	defer tx.Rollback(ctx)
	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()*/
	dbMocked := mockConnection(t)
	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()

	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()

	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()

	dbMocked.ExpectBegin()
	dbMocked.ExpectCommit()
	//dbMocked.ExpectRollback()

	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, dbMocked)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)
	nomadEventService.On("Save", mock.AnythingOfType("*pgxmock.pgxmock"), mock.AnythingOfType("*api.Event")).Return(errors.New(errorMessage))

	//when
	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), "Error Saving the Nomad event")
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}
