package service

import (
	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"log"
	"os"
)

type ActionService interface {
	GetById(uuid.UUID)(*model.ActionInstance, error)
	GetByNameAndWorkflowId(string, uint64)(*model.ActionInstance, error)
	GetAll()([]*model.ActionInstance, error)
	Save(pgx.Tx, *model.ActionInstance) error
	Update(pgx.Tx, uuid.UUID, *model.ActionInstance)(pgconn.CommandTag, error)
}

type ActionServiceImpl struct {
	logger *log.Logger
	actionRepository repository.ActionRepository
}

func NewActionService(db *pgxpool.Pool) ActionService {
	return &ActionServiceImpl{
		logger:           log.New(os.Stderr, "ActionService: ", log.LstdFlags),
		actionRepository: repository.NewActionRepository(db),
	}
}

func (cmd *ActionServiceImpl) GetById(id uuid.UUID) (action *model.ActionInstance, err error) {
	log.Printf("Get Action by id %s", id)
	action, err = cmd.actionRepository.GetById(id)
	if err != nil {
		log.Printf("Couldn't select existing Action for id %s: %s", id, err)
	}
	return action, err
}

func (cmd *ActionServiceImpl) GetByNameAndWorkflowId(name string, workflowId uint64) (action *model.ActionInstance, err error) {
	log.Printf("Get Action by name %s and workflow_instance_id %d", name, workflowId)
	action, err = cmd.actionRepository.GetByNameAndWorkflowId(name, workflowId)
	if err != nil {
		if !pgxscan.NotFound(err) {
			log.Printf("Couldn't select existing Action for name %s and workflow_instance_id %d: %s", name, workflowId, err)
		}
	}
	return action, err
}

func (cmd *ActionServiceImpl) GetAll() ([]*model.ActionInstance, error) {
	log.Printf("Get all Actions")
	return cmd.actionRepository.GetAll()
}

func (cmd *ActionServiceImpl) Save(tx pgx.Tx, action *model.ActionInstance) error {
	log.Printf("Saving new Action %#v", action)
	err := cmd.actionRepository.Save(tx, action)
	if err != nil {
		log.Printf("Couldn't insert Action: %s", err.Error())
	} else {
		log.Printf("Created Action %#v", action)
	}
	return err
}

func (cmd *ActionServiceImpl) Update(tx pgx.Tx, id uuid.UUID, action *model.ActionInstance) (pgconn.CommandTag, error) {
	log.Printf("Update Action %#v with id %s", action, id)
	commandTag, err := cmd.actionRepository.Update(tx, id, action)
	if err != nil {
		log.Printf("Couldn't update Action with id: %s, error: %s", id, err.Error())
	} else {
		log.Printf("Updated Action %#v with id %s, commandTag %s", action, id, commandTag)
	}
	return commandTag, err
}