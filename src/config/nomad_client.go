package config

import nomad "github.com/hashicorp/nomad/api"

func NomadClient() (nomadClient *nomad.Client, err error) {
	config := nomad.DefaultConfig()
	return nomad.NewClient(config)
}
