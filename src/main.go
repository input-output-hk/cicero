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

func NewDb() (db *pgxpool.Pool, err error) {
	db, err = model.DBConnection(os.Getenv("DATABASE_URL"))
	return
}

func NewNomadClient() (client *nomad.Client, err error) {
	client, err = nomad.NewClient(nomad.DefaultConfig())
	return
}
