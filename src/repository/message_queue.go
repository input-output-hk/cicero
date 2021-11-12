package repository

import (
	"context"
	"github.com/georgysavva/scany/pgxscan"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/liftbridge-io/go-liftbridge/v2"
)

type messageQueueRepository struct {
	DB *pgxpool.Pool
}

type MessageQueueRepository interface {
	GetOffset(string) (int64, error)
	Save(pgx.Tx, map[string][]byte, *liftbridge.Message) error
}

func NewMessageQueueRepository(db *pgxpool.Pool) MessageQueueRepository {
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
