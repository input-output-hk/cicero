package domain

import (
	"encoding/json"
	"fmt"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	cueformat "cuelang.org/go/cue/format"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
)

type InputDefinitionSelect uint

const (
	InputDefinitionSelectLatest InputDefinitionSelect = iota
	InputDefinitionSelectLatestNone
	InputDefinitionSelectAll
)

func (self *InputDefinitionSelect) String() (string, error) {
	switch *self {
	case InputDefinitionSelectLatest:
		return "latest", nil
	case InputDefinitionSelectLatestNone:
		return "latest_none", nil
	case InputDefinitionSelectAll:
		return "all", nil
	default:
		return "", fmt.Errorf("Unknown value %d", *self)
	}
}

func (self *InputDefinitionSelect) FromString(str string) error {
	switch str {
	case "latest":
		*self = InputDefinitionSelectLatest
	case "latest_none":
		*self = InputDefinitionSelectLatestNone
	case "all":
		*self = InputDefinitionSelectAll
	default:
		return fmt.Errorf("Unknown value %q", str)
	}
	return nil
}

func (self *InputDefinitionSelect) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}
	return self.FromString(str)
}

func (self InputDefinitionSelect) MarshalJSON() ([]byte, error) {
	if str, err := self.String(); err != nil {
		return nil, err
	} else {
		return json.Marshal(str)
	}
}

type InputDefinition struct {
	Select InputDefinitionSelect
	Match  cue.Value
}

type inputDefinitionJson struct {
	Select InputDefinitionSelect `json:"select"`
	Match  string                `json:"match"`
}

func (self *InputDefinition) UnmarshalJSON(data []byte) error {
	def := inputDefinitionJson{}
	if err := json.Unmarshal(data, &def); err != nil {
		return err
	}

	self.Select = def.Select
	self.Match = cuecontext.New().CompileString(def.Match)

	return nil
}

func (self InputDefinition) MarshalJSON() ([]byte, error) {
	def := inputDefinitionJson{}

	def.Select = self.Select

	if syntax, err := cueformat.Node(
		self.Match.Syntax(),
		cueformat.Simplify(),
	); err != nil {
		return nil, err
	} else {
		def.Match = string(syntax)
	}

	return json.Marshal(def)
}

func (self *InputDefinition) Scan(value interface{}) error {
	return self.UnmarshalJSON(value.([]byte))
}

type ActionDefinition struct {
	Meta    map[string]interface{}     `json:"meta"`
	Failure []interface{}              `json:"failure"`
	Success []interface{}              `json:"success"`
	Inputs  map[string]InputDefinition `json:"inputs"`
	Job     *nomad.Job                 `json:"job"`
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
	Inputs    map[string]InputDefinition
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
