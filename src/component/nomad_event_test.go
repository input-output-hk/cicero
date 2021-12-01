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

func TestStartWorkflowFailureToListenNomadEvent(t *testing.T) {
	t.Parallel()

	//given
	eventId := uint64(1)
	ctx := context.Background()
	stream := make(<-chan *nomad.Events, 0)
	nomadEventService := &mocks.NomadEventService{}
	nomadClient := &mocks.NomadClient{}
	nomadEventConsumer := buildNomadEventConsumerMocked(nomadEventService, nomadClient)
	nomadEventService.On("GetLastNomadEvent").Return(eventId, nil)
	errorMessage := "Some error"
	nomadClient.On("EventStream", ctx, eventId+1).Return(stream, errors.New(errorMessage))

	err := nomadEventConsumer.Start(ctx)

	//then
	assert.Equal(t, err.Error(), fmt.Errorf("Could not listen to Nomad events: %s", errorMessage).Error())
	nomadEventService.AssertExpectations(t)
	nomadClient.AssertExpectations(t)
}
