package cicero

import (
	"os"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgx/v4/pgxpool"
)

var BuildInfo struct {
	Version string
	Commit  string
}

var DB *pgxpool.Pool
var nomadClient *nomad.Client

func Init() error {
	var err error
	DB, err = model.DBConnection(os.Getenv("DATABASE_URL"))
	if err != nil {
		return err
	}
	config := nomad.DefaultConfig()
	nomadClient, err = nomad.NewClient(config)
	if err != nil {
		return err
	}

	return nil
}
