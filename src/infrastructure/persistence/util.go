package persistence

import (
	"context"
	"strconv"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/jackc/pgx/v4"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/util"
	"github.com/input-output-hk/cicero/src/domain/repository"
)

func fetchPage(
	db config.PgxIface,
	page *repository.Page,
	items interface{},
	selects, from, orderBy string,
	queryArgs ...interface{},
) error {
	batch := &pgx.Batch{}
	batch.Queue(`SELECT count(*) FROM `+from, queryArgs...)
	batch.Queue(
		`SELECT `+selects+
			` FROM `+from+
			` ORDER BY `+orderBy+
			` LIMIT $`+strconv.Itoa(len(queryArgs)+1)+
			` OFFSET $`+strconv.Itoa(len(queryArgs)+2),
		append(queryArgs, page.Limit, page.Offset)...,
	)

	br := db.SendBatch(context.Background(), batch)
	defer br.Close()

	if rows, err := br.Query(); err != nil {
		return err
	} else if err := util.ScanNextRow(rows, &page.Total); err != nil {
		return err
	}

	if rows, err := br.Query(); err != nil {
		return err
	} else if err := pgxscan.ScanAll(items, rows); err != nil {
		return err
	}

	return nil
}
