package cicero

import (
	"github.com/input-output-hk/cicero/src/domain"
	"os"

	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4/pgxpool"
)

func NewDb() (db *pgxpool.Pool, err error) {
	db, err = domain.DBConnection(os.Getenv("DATABASE_URL"))
	return
}

func NewNomadClient() (client *nomad.Client, err error) {
	client, err = nomad.NewClient(nomad.DefaultConfig())
	return
}
