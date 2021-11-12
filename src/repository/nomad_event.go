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
	GetEventAllocByWorkflowId(uint64) ([]map[string]interface{}, error)
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

func (n nomadEventRepository) GetEventAllocByWorkflowId(id uint64) (results []map[string]interface{}, err error) {
	err = pgxscan.Select(context.Background(), n.DB, &results, `
            SELECT name, payload->>'Allocation' AS alloc
            FROM (
              SELECT id, name
              FROM action_instances
              WHERE workflow_instance_id = $1
            ) action
            LEFT JOIN LATERAL (
              SELECT payload, index
              FROM nomad_events
              WHERE (payload#>>'{Allocation,JobID}')::uuid = action.id
              AND payload#>>'{Allocation,TaskGroup}' = action.name
              AND topic = 'Allocation'
              AND type = 'AllocationUpdated'
              ORDER BY index DESC LIMIT 1
            ) payload ON true;
            `, id)
	return
}
