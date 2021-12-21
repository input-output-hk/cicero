package application

import (
	"log"
	"os"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v4"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/config"
	"github.com/input-output-hk/cicero/src/domain"
	"github.com/input-output-hk/cicero/src/domain/repository"
	"github.com/input-output-hk/cicero/src/infrastructure/persistence"
)

type FactService interface {
	GetById(uuid.UUID) (domain.Fact, error)
	GetLatestByFields([][]string) (domain.Fact, error)
	GetByFields([][]string) ([]*domain.Fact, error)
	Save(pgx.Tx, *domain.Fact) error
	// TODO sometimes you need a Tx, sometimes not...
	// → SaveTx() and Save() etc? another wrapper? Tx() to get one?
}

type factService struct {
	logger         *log.Logger
	factRepository repository.FactRepository
}

func NewFactService(db config.PgxIface) FactService {
	return &factService{
		logger:         log.New(os.Stderr, "FactService: ", log.LstdFlags),
		factRepository: persistence.NewFactRepository(db),
	}
}

func (self *factService) GetById(id uuid.UUID) (fact domain.Fact, err error) {
	self.logger.Printf("Getting Fact by ID %s", id)
	fact, err = self.factRepository.GetById(id)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select existing Fact for ID: %s", id)
	}
	return
}

func (self *factService) Save(tx pgx.Tx, fact *domain.Fact) error {
	self.logger.Println("Saving new Fact")
	if err := self.factRepository.Save(tx, fact); err != nil {
		return errors.WithMessagef(err, "Could not insert Fact")
	}
	self.logger.Printf("Created Fact %s", fact.ID)
	return nil
}

func (self *factService) GetLatestByFields(fields [][]string) (fact domain.Fact, err error) {
	self.logger.Printf("Getting latest Facts by fields %q", fields)
	fact, err = self.factRepository.GetLatestByFields(fields)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select latest Facts by fields %q", fields)
	}
	return
}

func (self *factService) GetByFields(fields [][]string) (facts []*domain.Fact, err error) {
	self.logger.Printf("Getting Facts by fields %q", fields)
	facts, err = self.GetByFields(fields)
	if err != nil {
		err = errors.WithMessagef(err, "Could not select Facts by fields %q", fields)
	}
	return
}
