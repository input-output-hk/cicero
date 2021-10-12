package main

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/pkg/errors"
	"github.com/uptrace/bun"
	"github.com/uptrace/bun/dialect/sqlitedialect"
	"github.com/uptrace/bun/driver/sqliteshim"
)

type Api struct {
	db *bun.DB
}

var api = &Api{}

func init() {
	db, err := newDb()
	if err != nil {
		panic(err)
	}

	api = &Api{
		db: db,
	}
}

func (a *Api) Workflow(name string) (*workflowDefinition, error) {
	return nixInstantiateWorkflow(name, 0, "{}")
}

func (a *Api) WorkflowInstances(name string) ([]Workflow, error) {
	instances := []Workflow{}

	err := a.db.NewSelect().
		Model(&instances).
		Where("name = ?", name).
		Scan(context.Background())
	if err != nil { return nil, err }

	return instances, nil
}

func (a *Api) Workflows() (*workflowDefinitions, error) {
	return nixInstantiate("workflows", 0, "{}")
}

func (a *Api) WorkflowStart(name string) error {
	publish(fmt.Sprintf("workflow.%s.start", name), "workflow.*.start", map[string]interface{}{})
	return nil
}

func newDb() (*bun.DB, error) {
	sqldb, err := sql.Open(sqliteshim.ShimName, "db/database.sqlite3")
	if err != nil {
		return nil, errors.WithMessage(err, "While opening the DB")
	}

	db := bun.NewDB(sqldb, sqlitedialect.New())

	return db, nil
}
