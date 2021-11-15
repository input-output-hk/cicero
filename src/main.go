package cicero

import (
	nomad "github.com/hashicorp/nomad/api"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/jackc/pgx/v4/pgxpool"
	"os"
)

func Init() (db *pgxpool.Pool, nomadClient *nomad.Client, err error) {
	db, err = model.DBConnection(os.Getenv("DATABASE_URL"))
	if err != nil {
		return
	}

	nomadClient, err = nomad.NewClient(nomad.DefaultConfig())
	if err != nil {
		return
	}

	return
}
