package repository

import (
	"github.com/jackc/pgx/v4"
	"github.com/liftbridge-io/go-liftbridge/v2"
)

type MessageQueueRepository interface {
	GetOffset(string) (int64, error)
	Save(pgx.Tx, map[string][]byte, *liftbridge.Message) error
}
