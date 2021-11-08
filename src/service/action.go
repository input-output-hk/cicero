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

type ActionService interface {
	GetById(uint64)(model.ActionInstance, error)
	GetByNameAndWorkflowId(string, uint64)(model.ActionInstance, error)
	GetAll()([]*model.ActionInstance, error)
	Save(pgx.Tx, *model.ActionInstance) error
	Update(pgx.Tx, uint64, *model.ActionInstance)(pgconn.CommandTag, error)
}

type ActionServiceCmd struct {
	logger *log.Logger
	actionRepository repository.ActionRepository
}

func (cmd * ActionServiceCmd) Init(db *pgxpool.Pool) ActionService {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "ActionService: ", log.LstdFlags)
	}
	if cmd.actionRepository == nil {
		cmd.actionRepository = repository.NewActionRepository(db)
	}
	return cmd
}

func (cmd *ActionServiceCmd) GetById(id uint64) (action model.ActionInstance, err error) {
	log.Printf("Get Action by id %d", id)
	action, err = cmd.actionRepository.GetById(id)
	if err != nil {
		log.Printf("Couldn't select existing Action for id %d: %s", id, err)
	}
	return action, err
}

func (cmd *ActionServiceCmd) GetByNameAndWorkflowId(name string, workflowId uint64) (action model.ActionInstance, err error) {
	log.Printf("Get Action by name %s and workflow_instance_id %d", name, workflowId)
	action, err = cmd.actionRepository.GetByNameAndWorkflowId(name, workflowId)
	if err != nil {
		log.Printf("Couldn't select existing Action for name %s and workflow_instance_id %d: %s", name, workflowId, err)
	}
	return action, err
}

func (cmd *ActionServiceCmd) GetAll() ([]*model.ActionInstance, error) {
	log.Printf("Get all Actions")
	return cmd.actionRepository.GetAll()
}

func (cmd *ActionServiceCmd) Save(tx pgx.Tx, action *model.ActionInstance) error {
	log.Printf("Saving new Action %#v", action)
	err := cmd.actionRepository.Save(tx, action)
	if err != nil {
		log.Printf("Couldn't insert Action: %s", err.Error())
	} else {
		log.Printf("Created Action %#v", action)
	}
	return err
}

func (cmd *ActionServiceCmd) Update(tx pgx.Tx, id uint64, action *model.ActionInstance) (pgconn.CommandTag, error) {
	log.Printf("Update Action %#v with id %d", action, id)
	commandTag, err := cmd.actionRepository.Update(tx, id, action)
	if err != nil {
		log.Printf("Couldn't update Action with id: %d, error: %s", id, err.Error())
	} else {
		log.Printf("Updated Action %#v with id %d, commandTag %s", action, id, commandTag)
	}
	return commandTag, err
}