package cicero

import (
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgx/v4/pgxpool"
	"os"
)

var DB *pgxpool.Pool
var nomadClient *nomad.Client

func Init() error {
	var err error
	DB, err = model.DBConnection(os.Getenv("DATABASE_URL"))
	if err != nil {
		return err
	}

	nomadClient, err = nomad.NewClient(nomad.DefaultConfig())
	if err != nil {
		return err
	}

	return nil
}