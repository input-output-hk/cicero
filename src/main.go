package cicero

import (
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/uptrace/bun"
)

var DB *bun.DB
var nomadClient *nomad.Client

func Init() error {
	openedDB, err := model.DBConnection()
	if err != nil {
		return err
	}
	DB = openedDB

	nomadClient, err = nomad.NewClient(nomad.DefaultConfig())
	if err != nil {
		return err
	}

	return nil
}

