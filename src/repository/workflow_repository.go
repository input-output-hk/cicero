package repository

import (
	"context"
	"database/sql"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/uptrace/bun"
)

type WorkflowRepository interface {
	GetAllByName(string)([]model.WorkflowInstance, error)
	GetByNameAndId(string, uint64)(model.WorkflowInstance, error)
	GetById(uint64)(model.WorkflowInstance, error)
	Save(*bun.Tx, *model.WorkflowInstance)(sql.Result, error)
	Update(*bun.Tx, uint64, *model.WorkflowInstance)(sql.Result, error)
}

type workflowRepository struct {
	DB *bun.DB
}

func NewWorkflowRepository(db *bun.DB) WorkflowRepository {
	return workflowRepository{DB: db}
}

//TODO: why by name? Id is primary key
func (w workflowRepository) GetByNameAndId(name string, id uint64) (instance model.WorkflowInstance, err error) {
	err = w.DB.NewSelect().
		Model(&instance).
		Where("name = ? AND id = ?", name, id).
		Scan(context.Background())
	return instance, err
}

func (w workflowRepository) GetById(id uint64) (instance model.WorkflowInstance, err error) {
	err = w.DB.NewSelect().
		Model(&instance).
		Where("id = ?", id).
		Scan(context.Background())
	return instance, err
}

func (w workflowRepository) GetAllByName(name string) (instances []model.WorkflowInstance, err error) {
	err = w.DB.NewSelect().
		Model(&instances).
		Where("name = ?", name).
		Scan(context.Background())
	return instances, err
}

func (w workflowRepository) Update(tx *bun.Tx, id uint64, workflow *model.WorkflowInstance) (result sql.Result, err error) {
	result, err = tx.NewUpdate().
		Where("id = ?", id).
		Model(workflow).
		Exec(context.Background())

	return result, err
}

func (w workflowRepository) Save(tx *bun.Tx, workflow *model.WorkflowInstance) (result sql.Result, err error) {
	result, err = tx.NewInsert().
		Model(workflow).
		Exec(context.Background())

	return result, err
}