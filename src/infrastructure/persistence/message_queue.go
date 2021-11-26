package persistence

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
)

type messageQueueRepository struct {
	DB config.PgxIface
}

func NewMessageQueueRepository(db config.PgxIface) repository.MessageQueueRepository {
	return messageQueueRepository{db}
}

func (m messageQueueRepository) GetOffset(streamName string) (offset int64, err error) {
	err = pgxscan.Get(
		context.Background(), m.DB, &offset,
		`SELECT COALESCE(MAX("offset") + 1, 0) FROM liftbridge_messages WHERE stream = $1`,
		streamName,
	)
	return
}

func (m messageQueueRepository) Save(tx pgx.Tx, headers map[string][]byte, msg *liftbridge.Message) (err error) {
	_, err = tx.Exec(
		context.Background(),
		`INSERT INTO liftbridge_messages ("offset", headers, stream, subject, created_at, value) VALUES ($1, $2, $3, $4, $5, $6)`,
		msg.Offset(), headers, msg.Stream(), msg.Subject(), msg.Timestamp(), msg.Value(),
	)
	return
}
