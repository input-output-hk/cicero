package application

import (
	"context"
	nomad "github.com/hashicorp/nomad/api"
)

type NomadClient interface {
	EventStream(ctx context.Context, index uint64) (<-chan *nomad.Events, error)
	JobsRegister(job *nomad.Job, q *nomad.WriteOptions) (*nomad.JobRegisterResponse, *nomad.WriteMeta, error)
	JobsDeregister(jobID string, purge bool, q *nomad.WriteOptions) (string, *nomad.WriteMeta, error)
}

type nomadClient struct {
	nClient           *nomad.Client
}

func NewNomadClient(nClient *nomad.Client) NomadClient {
	return &nomadClient{
		nClient: nClient,
	}
}

func (self *nomadClient) EventStream(ctx context.Context, nomadIndex uint64) (<-chan *nomad.Events, error) {
	return self.nClient.EventStream().Stream(
		ctx,
		map[nomad.Topic][]string{
			nomad.TopicDeployment: {string(nomad.TopicAll)},
			nomad.TopicEvaluation: {string(nomad.TopicAll)},
			nomad.TopicAllocation: {string(nomad.TopicAll)},
			nomad.TopicJob:        {string(nomad.TopicAll)},
			nomad.TopicNode:       {string(nomad.TopicAll)},
		},
		nomadIndex,
		nil,
	)
}

func (self *nomadClient) JobsRegister(job *nomad.Job, q *nomad.WriteOptions) (*nomad.JobRegisterResponse, *nomad.WriteMeta, error) {
	return self.nClient.Jobs().Register(job, q)
}

func (self *nomadClient) JobsDeregister(jobID string, purge bool, q *nomad.WriteOptions) (string, *nomad.WriteMeta, error) {
	return self.nClient.Jobs().Deregister(jobID, purge, q)
}