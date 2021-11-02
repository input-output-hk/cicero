package repository

import (
	"context"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/uptrace/bun"
	"log"
)

type WorkflowRepository interface {
	GetAllByName(string)([]model.WorkflowInstance, error)
	GetAllByNameAndId(string, uint64)(model.WorkflowInstance, error)
	WithTrx(*bun.DB) workflowRepository
}

type workflowRepository struct {
	DB *bun.DB
}

func NewWorkflowRepository(db *bun.DB) WorkflowRepository {
	return workflowRepository{DB: db}
}

//TODO: why by name? Id is primary key
func (w workflowRepository) GetAllByNameAndId(name string, id uint64) (instance model.WorkflowInstance, err error) {
	err = w.DB.NewSelect().
		Model(&instance).
		Where("name = ? AND id = ?", name, id).
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

func (w workflowRepository) WithTrx(trxHandle *bun.DB) workflowRepository {
	if trxHandle == nil {
		log.Print("Transaction DataBase not found")
		return w
	}
	w.DB = trxHandle
	return w
}
