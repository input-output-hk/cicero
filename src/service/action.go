package service

import (
	"log"
	"os"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/google/uuid"
	"github.com/input-output-hk/cicero/src/model"
	"github.com/input-output-hk/cicero/src/repository"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/pkg/errors"
)

type ActionService interface {
	GetById(uuid.UUID) (*model.ActionInstance, error)
	GetByNameAndWorkflowId(string, uint64) (*model.ActionInstance, error)
	GetAll() ([]*model.ActionInstance, error)
	Save(pgx.Tx, *model.ActionInstance) error
	Update(pgx.Tx, uuid.UUID, *model.ActionInstance) error
}

type ActionServiceImpl struct {
	logger           *log.Logger
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
		err = errors.WithMessagef(err, "Couldn't select existing Action for id %s: %s", id)
	}
	return
}

func (cmd *ActionServiceImpl) GetByNameAndWorkflowId(name string, workflowId uint64) (action *model.ActionInstance, err error) {
	log.Printf("Get Action by name %s and workflow_instance_id %d", name, workflowId)
	action, err = cmd.actionRepository.GetByNameAndWorkflowId(name, workflowId)
	if err != nil && !pgxscan.NotFound(err) {
		err = errors.WithMessagef(err, "Couldn't select existing Action for name %s and workflow_instance_id %d", name, workflowId)
	}
	return
}

func (cmd *ActionServiceImpl) GetAll() ([]*model.ActionInstance, error) {
	log.Printf("Get all Actions")
	return cmd.actionRepository.GetAll()
}

func (cmd *ActionServiceImpl) Save(tx pgx.Tx, action *model.ActionInstance) error {
	log.Printf("Saving new Action %#v", action)
	if err := cmd.actionRepository.Save(tx, action); err != nil {
		return errors.WithMessagef(err, "Couldn't insert Action")
	}
	log.Printf("Created Action %#v", action)
	return nil
}

func (cmd *ActionServiceImpl) Update(tx pgx.Tx, id uuid.UUID, action *model.ActionInstance) error {
	log.Printf("Update Action %#v with id %s", action, id)
	if err := cmd.actionRepository.Update(tx, id, action); err != nil {
		return errors.WithMessagef(err, "Couldn't update Action with id: %s", id)
	}
	log.Printf("Updated Action %#v with id %s", action, id)
	return nil
}
