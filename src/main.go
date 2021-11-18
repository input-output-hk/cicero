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

func Init() (db *pgxpool.Pool, nomadClient *nomad.Client, err error) {
	db, err = model.DBConnection(os.Getenv("DATABASE_URL"))
	if err != nil {
		return
	}
	config := nomad.DefaultConfig()
	nomadClient, err = nomad.NewClient(config)
	if err != nil {
		return
	}

	return
}
