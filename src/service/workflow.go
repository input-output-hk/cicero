package service

import (
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"log"
	"os"
)

type WorkflowService interface {
	GetAllByName(string)([]*model.WorkflowInstance, error)
	GetById(uint64)(model.WorkflowInstance, error)
	Save(pgx.Tx, *model.WorkflowInstance) error
	Update(pgx.Tx, uint64, *model.WorkflowInstance)(pgconn.CommandTag, error)
}

type WorkflowServiceCmd struct {
	logger *log.Logger
	workflowRepository repository.WorkflowRepository
}

func (cmd *WorkflowServiceCmd) Init(db *pgxpool.Pool) WorkflowService {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "WorkflowService: ", log.LstdFlags)
	}
	if cmd.workflowRepository == nil {
		cmd.workflowRepository = repository.NewWorkflowRepository(db)
	}
	return cmd
}

func (cmd *WorkflowServiceCmd) GetAllByName(name string) ([]*model.WorkflowInstance, error) {
	log.Printf("Get all Workflows by name %s", name)
	return cmd.workflowRepository.GetAllByName(name)
}

func (cmd *WorkflowServiceCmd) GetById(id uint64) (workflow model.WorkflowInstance, err error) {
	log.Printf("Get workflow by id %d", id)
	workflow, err = cmd.workflowRepository.GetById(id)
	if err != nil {
		log.Printf("Couldn't select existing workflow for id %d: %s\n", id, err)
	}
	return workflow, err
}

func (cmd *WorkflowServiceCmd) Save(tx pgx.Tx, workflow *model.WorkflowInstance) error {
	log.Printf("Saving new workflow %#v", workflow)
	err := cmd.workflowRepository.Save(tx, workflow)
	if err != nil {
		log.Printf("Couldn't insert workflow: %s", err.Error())
	} else {
		log.Printf("Created workflow %#v", workflow)
	}
	return err
}

func (cmd *WorkflowServiceCmd) Update(tx pgx.Tx, id uint64, workflow *model.WorkflowInstance) (commandTag pgconn.CommandTag, err error) {
	log.Printf("Update workflow %#v with id %d", workflow, id)
	commandTag, err = cmd.workflowRepository.Update(tx, id, workflow)
	if err != nil {
		log.Printf("Couldn't update workflow with id: %d, error: %s", id, err.Error())
	} else {
		log.Printf("Updated workflow %#v with id %d, commandTag %s", workflow, id, commandTag)
	}
	return commandTag, err
}
