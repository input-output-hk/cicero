package domain

import (
	"encoding/json"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
)

type InputsDefinition struct {
	Latest     map[string]cue.Value
	LatestNone map[string]cue.Value
	All        map[string]cue.Value

	// XXX Do we really need to keep this alive?
	cueContext *cue.Context
}

func (self *InputsDefinition) UnmarshalJSON(data []byte) error {
	def := struct {
		Latest     map[string]string `json:"latest"`
		LatestNone map[string]string `json:"latest_none"`
		All        map[string]string `json:"all"`
	}{}
	if err := json.Unmarshal(data, &def); err != nil {
		return err
	}

	self.cueContext = cuecontext.New()
	for k, v := range def.Latest {
		self.Latest[k] = self.cueContext.CompileString(v)
	}
	for k, v := range def.LatestNone {
		self.LatestNone[k] = self.cueContext.CompileString(v)
	}
	for k, v := range def.All {
		self.All[k] = self.cueContext.CompileString(v)
	}

	return nil
}

func (self *InputsDefinition) Scan(value interface{}) error {
	return self.UnmarshalJSON(value.([]byte))
}

type ActionDefinition struct {
	Name    string                 `json:"name"`
	Meta    map[string]interface{} `json:"meta"`
	Failure []interface{}          `json:"failure"`
	Success []interface{}          `json:"success"`
	Inputs  InputsDefinition       `json:"inputs"`
	Job     *nomad.Job             `json:"job"`
}

func (s *ActionDefinition) IsDecision() bool {
	return s.Job == nil
}

type Fact struct {
	ID         uuid.UUID   `json:"id"`
	RunId      *uuid.UUID  `json:"run_id"`
	CreatedAt  time.Time   `json:"time"`
	Value      interface{} `json:"value"`
	BinaryHash *string     `json:"binary_hash"`
	// TODO too expensive to pass around, get from DB
	// TODO hash using https://pkg.go.dev/github.com/direnv/direnv/v2/sri
	// Binary     *[]byte     `json:"-"`
}

type Action struct {
	ID        uuid.UUID
	Meta      map[string]interface{}
	Name      string
	Source    string
	Inputs    InputsDefinition
	CreatedAt time.Time
}

type Run struct {
	NomadJobID uuid.UUID
	ActionId   uuid.UUID
	Failure    []interface{}
	Success    []interface{}
	CreatedAt  time.Time
	FinishedAt *time.Time
}

// TODO make this a map[string]struct{…}
// TODO move this to web package
type ActionSummary []struct {
	Name       string
	NumSources uint64
	Num        uint64
}
