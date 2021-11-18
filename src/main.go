package cicero

import (
	"github.com/input-output-hk/cicero/src/config"
	"os"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4/pgxpool"
)

func Init() (db *pgxpool.Pool, nomadClient *nomad.Client, err error) {
	db, err = config.DBConnection(os.Getenv("DATABASE_URL"))
	if err != nil {
		return
	}
	nomadClient, err = config.NomadClient()
	if err != nil {
		return
	}
	return
}
