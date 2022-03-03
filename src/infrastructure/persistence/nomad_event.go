package persistence

import (
	"context"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

type nomadEventRepository struct {
	DB config.PgxIface
}

func NewNomadEventRepository(db config.PgxIface) repository.NomadEventRepository {
	return nomadEventRepository{db}
}

func (n nomadEventRepository) WithQuerier(querier config.PgxIface) repository.NomadEventRepository {
	return nomadEventRepository{querier}
}

func (n nomadEventRepository) Save(event *nomad.Event) (err error) {
	_, err = n.DB.Exec(
		context.Background(),
		`INSERT INTO nomad_event (topic, "type", "key", filter_keys, "index", payload) VALUES ($1, $2, $3, $4, $5, $6)`,
		event.Topic, event.Type, event.Key, event.FilterKeys, event.Index, event.Payload,
	)
	return
}

func (n nomadEventRepository) GetLastNomadEventIndex() (index uint64, err error) {
	err = pgxscan.Get(
		context.Background(), n.DB, &index,
		`SELECT COALESCE(MAX("index"), 0) FROM nomad_event`,
	)
	return
}

func (n nomadEventRepository) GetEventAllocByNomadJobId(id uuid.UUID) (results []map[string]interface{}, err error) {
	err = pgxscan.Select(context.Background(), n.DB, &results, `
		SELECT "index", payload->>'Allocation' AS alloc
		FROM nomad_event
		WHERE payload#>>'{Allocation,JobID}' = $1
			AND topic = 'Allocation'
			AND type = 'AllocationUpdated'
		ORDER BY "index" ASC
	`, id)
	return
}
