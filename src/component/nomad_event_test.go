package component

import (
	"context"
	"errors"
	"fmt"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/application"
	"github.com/jackc/pgx/v4"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"log"
	"os"
	"testing"
)

type NomadEventServiceMocked struct {
	mock.Mock
}

func (n *NomadEventServiceMocked) Save(tx pgx.Tx, event *nomad.Event) error {
	panic("implement me")
}

func (n *NomadEventServiceMocked) GetLastNomadEvent() (event uint64, err error) {
	args := n.Called()
	if args.Get(0) != nil {
		event = args.Get(0).(uint64)
	}
	err = args.Error(1)
	return
}

func (n *NomadEventServiceMocked) GetEventAllocByWorkflowId(u uint64) (map[string]application.AllocWrapper, error) {
	panic("implement me")
}

type NomadClientMocked struct {
	mock.Mock
}

func (n *NomadClientMocked) EventStream(ctx context.Context, index uint64) (stream <-chan *nomad.Events, err error) {
	args := n.Called(ctx, index)
	if args.Get(0) != nil {
		stream = make(chan *nomad.Events, 0)
	}
	err = args.Error(1)
	return
}

func (n *NomadClientMocked) JobsRegister(job *nomad.Job, q *nomad.WriteOptions) (*nomad.JobRegisterResponse, *nomad.WriteMeta, error) {
	panic("implement me")
}

func (n *NomadClientMocked) JobsDeregister(jobID string, purge bool, q *nomad.WriteOptions) (string, *nomad.WriteMeta, error) {
	panic("implement me")
}

func buildNomadEventConsumerMocked(nomadEventServiceMocked *NomadEventServiceMocked, nomadClientMocked *NomadClientMocked) *NomadEventConsumer {
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
	stream := make(chan *nomad.Events, 1)
	nomadEventService := new(NomadEventServiceMocked)
	nomadClient := new(NomadClientMocked)
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
