package repository

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type nomadEventRepository struct {
	DB *pgxpool.Pool
}

type NomadEventRepository interface {
	Save(pgx.Tx, *nomad.Event) error
	GetLastNomadEvent() (uint64, error)
}

func NewNomadEventRepository(db *pgxpool.Pool) NomadEventRepository {
	return nomadEventRepository{DB: db}
}

func (n nomadEventRepository) Save(tx pgx.Tx, event *nomad.Event) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`INSERT INTO nomad_events (topic, "type", "key", filter_keys, "index", payload) VALUES ($1, $2, $3, $4, $5, $6)`,
		event.Topic, event.Type, event.Key, event.FilterKeys, event.Index, event.Payload,
	)
	return
}

func (n nomadEventRepository) GetLastNomadEvent() (index uint64, err error) {
	err = pgxscan.Get(
		context.Background(), n.DB, &index,
		`SELECT COALESCE(MAX("index") + 1, 0) FROM nomad_events`,
	)
	return
}