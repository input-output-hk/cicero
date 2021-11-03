package service

import (
	"database/sql"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/uptrace/bun"
	"log"
	"os"
)

type WorkflowService interface {
	GetAllByName(string)([]model.WorkflowInstance, error)
	GetByNameAndId(string, uint64)(model.WorkflowInstance, error)
	GetById(uint64)(model.WorkflowInstance, error)
	Save(*bun.Tx, *model.WorkflowInstance)(sql.Result, error)
	Update(*bun.Tx, uint64, *model.WorkflowInstance)(sql.Result, error)
}

type WorkflowServiceCmd struct {
	logger *log.Logger
	workflowRepository repository.WorkflowRepository
}

func (cmd *WorkflowServiceCmd) Init(db *bun.DB) WorkflowService {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "WorkflowService: ", log.LstdFlags)
	}
	if cmd.workflowRepository == nil {
		cmd.workflowRepository = repository.NewWorkflowRepository(db)
	}
	return cmd
}

func (cmd *WorkflowServiceCmd) GetAllByName(name string) ([]model.WorkflowInstance, error) {
	log.Printf("Get all Workflows by name %s", name)
	return cmd.workflowRepository.GetAllByName(name)
}

func (cmd *WorkflowServiceCmd) GetByNameAndId(name string, id uint64) (workflow model.WorkflowInstance, err error) {
	log.Printf("Get all Workflows by name %s and id %d", name, id)
	workflow, err = cmd.workflowRepository.GetByNameAndId(name, id)
	if err != nil {
		log.Printf("Couldn't select existing workflow for id %d and name %s: %s", id, name, err)
	}
	return workflow, err
}
func (cmd *WorkflowServiceCmd) GetById(id uint64) (workflow model.WorkflowInstance, err error) {
	log.Printf("Get all Workflows by id %d", id)
	workflow, err = cmd.workflowRepository.GetById(id)
	if err != nil {
		log.Printf("Couldn't select existing workflow for id %d: %s\n", id, err)
	}
	return workflow, err
}

func (cmd *WorkflowServiceCmd) Save(tx *bun.Tx, workflow *model.WorkflowInstance) (result sql.Result, err error) {
	log.Printf("Saving new workflow %#v", workflow)
	result, err = cmd.workflowRepository.Save(tx, workflow)
	if err != nil {
		log.Printf("%#v %#v", result, err)
		log.Printf("Couldn't insert workflow: %s", err.Error())
	} else {
		log.Printf("Created workflow %#v", workflow)
	}
	return result, err
}

func (cmd *WorkflowServiceCmd) Update(tx *bun.Tx, id uint64, workflow *model.WorkflowInstance) (result sql.Result, err error) {
	log.Printf("Update workflow %#v with id %d", workflow, id)
	result, err = cmd.workflowRepository.Update(tx, id, workflow)
	if err != nil {
		log.Printf("Couldn't update workflow with id: %d, error: %s", id, err.Error())
	} else {
		log.Printf("Updated workflow %#v with id %d", workflow, id)
	}
	return result, err
}
