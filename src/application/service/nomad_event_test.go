package service

import (
	"context"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/rs/zerolog/log"
	"github.com/stretchr/testify/assert"
	"testing"

	dbMocks "github.com/input-output-hk/cicero/src/config/mocks"
	mocks "github.com/input-output-hk/cicero/src/mocks/application/service"
	repositoryMocks "github.com/input-output-hk/cicero/src/mocks/domain/repository"
)

func buildNomadEventService(
	nomadEventRepository *repositoryMocks.NomadEventRepository,
	runService *mocks.RunService) *nomadEventService {
	return &nomadEventService{
		logger:               log.Logger,
		nomadEventRepository: nomadEventRepository,
		runService:           runService,
	}
}

func TestSavingNomadEventSuccess(t *testing.T) {
	t.Parallel()

	// given
	event := &nomad.Event{}
	eventNomadRepository := &repositoryMocks.NomadEventRepository{}
	runService := &mocks.RunService{}
	nomadEventService := buildNomadEventService(eventNomadRepository, runService)
	_, tx := dbMocks.BuildTransaction(context.Background(), t)
	eventNomadRepository.On("Save", tx, event).Return(nil)

	// when
	err := nomadEventService.Save(tx, event)

	// then
	assert.Equal(t, nil, err, "No error")
	eventNomadRepository.AssertExpectations(t)
}
