package config

import nomad "github.com/hashicorp/nomad/api"

func NewNomadClient() (client *nomad.Client, err error) {
	config := nomad.DefaultConfig()
	//TODO: log configuration
	client, err = nomad.NewClient(config)
	return
}