package component

import (
	"context"
	"errors"
	"fmt"
	"log"
	"os"
	"testing"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application/mocks"
	configMocks "github.com/input-output-hk/cicero/src/config/mocks"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func buildNomadEventConsumerMocked(nomadEventServiceMocked *mocks.NomadEventService,
	nomadClientMocked *mocks.NomadClient, db *configMocks.PgxIface) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:            log.New(os.Stderr, "NomadEventConsumerTest: ", log.LstdFlags),
		NomadEventService: nomadEventServiceMocked,
		NomadClient:       nomadClientMocked,
		Db:                db,
	}
}

func generateEvents(events []nomad.Events) <-chan *nomad.Events {
	rc := make(chan *nomad.Events, len(events))
	go func() {
		defer close(rc)
		for _, event := range events {
			rc <- &event
		}
	}()
	return rc
}

func TestStartWorkflowFailureToListenNomadEvent(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	stream := generateEvents([]nomad.Events{})
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	errorMessage := "Some error"
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, errors.New(errorMessage))

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not listen to Nomad events: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureGettingNextEvents(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	events := []nomad.Events{
		{Err: errors.New(errorMessage)},
	}
	stream := generateEvents(events)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Error getting next events from Nomad event stream: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowGettingEventAllocationFailure(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation", Type: "AllocationUpdated"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation", Type: "AllocationUpdated", Payload: map[string]interface{}{
		"Job": "dsf2",
	}}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(events)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, nil)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Contains(t, err.Error(), "Error handling Nomad event")
	assert.Contains(t, err.Error(), "Error getting Nomad event's allocation")
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureToSaveEvent(t *testing.T) {
	t.Parallel()

	// given
	eventId := uint64(1)
	ctx := context.Background()
	errorMessage := "Some error"
	event1 := nomad.Events{Index: uint64(1), Events: []nomad.Event{{Topic: "Allocation"}}}
	event2 := nomad.Events{Index: uint64(2), Events: []nomad.Event{{Topic: "Allocation"}}}
	events := []nomad.Events{event1, event2}
	stream := generateEvents(events)

	db := &configMocks.PgxIface{}
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient, db)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)
	db.On("BeginFunc", ctx, mock.AnythingOfType("func(pgx.Tx) error")).Return(errors.New(errorMessage))

	// when
	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Error Saving the Nomad event: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
	db.AssertExpectations(t)
}
