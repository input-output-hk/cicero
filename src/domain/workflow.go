package domain

import (
	"encoding/json"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	cueformat "cuelang.org/go/cue/format"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
)

type InputsDefinition struct {
	Latest     map[string]cue.Value
	LatestNone map[string]cue.Value
	All        map[string]cue.Value
}

type inputsDefinitionStringly struct {
	Latest     map[string]string `json:"latest"`
	LatestNone map[string]string `json:"latest_none"`
	All        map[string]string `json:"all"`
}

func (self *InputsDefinition) UnmarshalJSON(data []byte) error {
	def := inputsDefinitionStringly{}
	if err := json.Unmarshal(data, &def); err != nil {
		return err
	}

	ctx := cuecontext.New()

	convert := func(from *map[string]string) (into map[string]cue.Value) {
		into = map[string]cue.Value{}
		for k, v := range *from {
			into[k] = ctx.CompileString(v)
		}
		return
	}

	self.Latest = convert(&def.Latest)
	self.LatestNone = convert(&def.LatestNone)
	self.All = convert(&def.All)

	return nil
}

func (self InputsDefinition) MarshalJSON() ([]byte, error) {
	def := inputsDefinitionStringly{
		Latest:     map[string]string{},
		LatestNone: map[string]string{},
		All:        map[string]string{},
	}

	convert := func(from *map[string]cue.Value, into *map[string]string) error {
		for k, v := range *from {
			if syntax, err := cueformat.Node(
				v.Syntax(),
				cueformat.Simplify(),
			); err != nil {
				return err
			} else {
				(*into)[k] = string(syntax)
			}
		}
		return nil
	}

	convert(&self.Latest, &def.Latest)
	convert(&self.LatestNone, &def.LatestNone)
	convert(&self.All, &def.All)

	return json.Marshal(def)
}

func (self *InputsDefinition) Scan(value interface{}) error {
	return self.UnmarshalJSON(value.([]byte))
}

type ActionDefinition struct {
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
