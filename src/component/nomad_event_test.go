package component

import (
	"context"
	"errors"
	"fmt"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application/mocks"
	"github.com/stretchr/testify/assert"
	"log"
	"os"
	"testing"
)

func buildNomadEventConsumerMocked(nomadEventServiceMocked *mocks.NomadEventService, nomadClientMocked *mocks.NomadClient) *NomadEventConsumer {
	return &NomadEventConsumer{
		Logger:            log.New(os.Stderr, "NomadEventConsumerTest: ", log.LstdFlags),
		NomadEventService: nomadEventServiceMocked,
		NomadClient:       nomadClientMocked,
	}
}

func generateEvents(events []nomad.Events) <-chan *nomad.Events {
	rc := make(chan *nomad.Events)
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
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	errorMessage := "Some error"
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, errors.New(errorMessage))

	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not listen to Nomad events: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}

func TestStartWorkflowFailureWithEventError(t *testing.T) {
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
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, nil)

	err := nomadEventConsumer.Start(ctx)

	// then
	assert.Equal(t, err.Error(), fmt.Errorf("Error getting next events from Nomad event stream: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}
