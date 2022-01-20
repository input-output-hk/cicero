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
	InputDefinitionSelectAll
)

func (self *InputDefinitionSelect) String() (string, error) {
	switch *self {
	case InputDefinitionSelectLatest:
		return "latest", nil
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

type InputDefinitionMatch string

func (self *InputDefinitionMatch) WithInputs(inputs map[string]interface{}) cue.Value {
	ctx := cuecontext.New()
	return ctx.CompileString(
		string(*self),
		cue.Scope(ctx.Encode(struct{}{}).
			// XXX check which inputs are actually used and pass in only those
			FillPath(cue.MakePath(cue.Hid("_inputs", "_")), inputs),
		),
	)
}

func (self *InputDefinitionMatch) WithoutInputs() cue.Value {
	return self.WithInputs(map[string]interface{}{})
}

func (self *InputDefinitionMatch) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}

	match := InputDefinitionMatch(str)
	if err := match.WithoutInputs().Err(); err != nil {
		return err
	}

	*self = match
	return nil
}

// Makes sure to be valid CUE that does not evaluate to an error
// by marshaling the parsed syntax instead of the raw string.
func (self InputDefinitionMatch) MarshalJSON() ([]byte, error) {
	match := self.WithoutInputs()
	if err := match.Err(); err != nil {
		return nil, err
	} else if syntax, err := cueformat.Node(
		match.Syntax(
			cue.Hidden(true),
			cue.Optional(true),
			cue.ResolveReferences(false),
		),
		cueformat.Simplify(),
	); err != nil {
		return nil, err
	} else {
		return json.Marshal(string(syntax))
	}
}

func (self *InputDefinitionMatch) Scan(value interface{}) error {
	return self.UnmarshalJSON(value.([]byte))
}

type InputDefinition struct {
	Select   InputDefinitionSelect `json:"select"`
	Not      bool                  `json:"not"`
	Optional bool                  `json:"optional"`
	Match    InputDefinitionMatch  `json:"match"`
}

type ActionDefinition struct {
	Meta   map[string]interface{}     `json:"meta"`
	Inputs map[string]InputDefinition `json:"inputs"`
}

type RunOutput struct {
	Failure *interface{} `json:"failure"`
	Success *interface{} `json:"success"`
}

type RunDefinition struct {
	Output RunOutput  `json:"output"`
	Job    *nomad.Job `json:"job"`
}

func (s *RunDefinition) IsDecision() bool {
	return s.Job == nil
}

type Fact struct {
	ID         uuid.UUID   `json:"id"`
	RunId      *uuid.UUID  `json:"run_id,omitempty"`
	CreatedAt  time.Time   `json:"created_at"`
	Value      interface{} `json:"value"`
	BinaryHash *string     `json:"binary_hash,omitempty"`
	// TODO nyi: unique key over (value, binary_hash)?
}

type Action struct {
	ID        uuid.UUID `json:"id"`
	Name      string    `json:"name"`
	Source    string    `json:"source"`
	CreatedAt time.Time `json:"created_at"`
	ActionDefinition
}

type Run struct {
	NomadJobID uuid.UUID  `json:"nomad_job_id"`
	ActionId   uuid.UUID  `json:"action_id"`
	CreatedAt  time.Time  `json:"created_at"`
	FinishedAt *time.Time `json:"finished_at"`
}
