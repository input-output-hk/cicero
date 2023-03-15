package domain

import (
	"encoding/json"
	"fmt"
	"io"
	"time"

	"cuelang.org/go/cue"
	cueliteral "cuelang.org/go/cue/literal"
	"cuelang.org/go/tools/flow"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/util"
)

type Action struct {
	ID        uuid.UUID `json:"id"`
	Name      string    `json:"name"`
	Source    string    `json:"source"`
	CreatedAt time.Time `json:"created_at"`
	ActionDefinition
}

type ActionDefinition struct {
	Meta  map[string]any `json:"meta"`
	InOut InOutCUEString `json:"io" db:"io"`
}

type ActionSatisfaction struct {
	ActionId uuid.UUID `json:"action_id"`
	Input    string    `json:"input"`
	FactId   uuid.UUID `json:"fact_id"`
}

type InOutCUEString util.CUEString

type InputDefinitions map[string]InputDefinition

type InputDefinition struct {
	Not      bool
	Optional bool
	Match    cue.Value
}

type OutputDefinition struct {
	Success cue.Value
	Failure cue.Value
}

type Invocation struct {
	Id         uuid.UUID  `json:"id"`
	ActionId   uuid.UUID  `json:"action_id"`
	CreatedAt  time.Time  `json:"created_at"`
	FinishedAt *time.Time `json:"finished_at"`
}

type Run struct {
	NomadJobID   uuid.UUID  `json:"nomad_job_id"`
	InvocationId uuid.UUID  `json:"invocation_id"`
	CreatedAt    time.Time  `json:"created_at"`
	FinishedAt   *time.Time `json:"finished_at"`
	Status       RunStatus  `json:"status"`
}

type RunStatus int8

const (
	RunStatusRunning RunStatus = iota
	RunStatusSucceeded
	RunStatusFailed
	RunStatusCanceled
)

type Fact struct {
	ID         uuid.UUID  `json:"id"`
	RunId      *uuid.UUID `json:"run_id,omitempty"`
	CreatedAt  time.Time  `json:"created_at"`
	Value      any        `json:"value"`
	BinaryHash *string    `json:"binary_hash,omitempty"`
	// TODO nyi: unique key over (value, binary_hash)?
}

type NomadEvent struct {
	nomad.Event
	Uid     util.MD5Sum
	Handled bool
}

func (self InOutCUEString) Inputs(inputs map[string]Fact) (InputDefinitions, error) {
	defs := InputDefinitions{}

	value := self.valueWithInputs(inputs)

	if fields, err := value.LookupPath(cue.MakePath(cue.Str("inputs"))).Fields(); err != nil {
		return nil, err
	} else {
		for fields.Next() {
			if def, err := self.Input(fields.Label(), inputs); err != nil {
				return nil, err
			} else if def != nil {
				defs[fields.Label()] = *def
			}
		}
	}

	return defs, nil
}

func (self InOutCUEString) Input(name string, inputs map[string]Fact) (*InputDefinition, error) {
	def := InputDefinition{}

	value := self.valueWithInputs(inputs).LookupPath(cue.MakePath(cue.Str("inputs"), cue.Str(name)))

	if v := value.LookupPath(cue.MakePath(cue.Str("not"))); v.Exists() {
		if not, err := v.Bool(); err != nil {
			return nil, err
		} else {
			def.Not = not
		}
	} else {
		def.Not = false
	}

	if v := value.LookupPath(cue.MakePath(cue.Str("optional"))); v.Exists() {
		if optional, err := v.Bool(); err != nil {
			return nil, err
		} else {
			def.Optional = optional
		}
	} else {
		def.Optional = false
	}

	if v := value.LookupPath(cue.MakePath(cue.Str("match"))); !v.Exists() {
		return nil, fmt.Errorf(`input %q must have a "match" field`, name)
	} else {
		def.Match = v
	}

	return &def, nil
}

func (self InOutCUEString) InputsFlow(runnerFunc flow.RunnerFunc) (*flow.Controller, error) {
	value := util.CUEString(self).Value(nil, nil)

	if err := value.Err(); err != nil {
		return nil, errors.WithMessage(err, "Bad io")
	}

	return flow.New(
		&flow.Config{
			Root: cue.MakePath(cue.Str("inputs")),
			UpdateFunc: func(c *flow.Controller, t *flow.Task) error {
				if t != nil {
					if err := t.Err(); err != nil {
						return errors.WithMessage(err, "UpdateFunc noticed task error")
					}
					if err := t.Value().Err(); err != nil {
						return errors.WithMessage(err, "UpdateFunc noticed task value is an error")
					}
				}
				return nil
			},
		},
		value,
		func(v cue.Value) (flow.Runner, error) {
			if n := len(v.Path().Selectors()); n > 2 {
				return nil, fmt.Errorf("Flow task path consists of more than two selectors: %s", v.Path())
			} else if n == 1 {
				// top-level inputs selector is not a task
				return nil, nil
			}
			return inputsFlowRunner(runnerFunc), nil
		},
	), nil
}

type inputsFlowRunner flow.RunnerFunc

func (self inputsFlowRunner) Run(t *flow.Task, err error) error {
	if err != nil {
		return errors.WithMessage(err, "Flow runner received error")
	}
	return self(t)
}

func InputName(task *flow.Task) string {
	name := task.Path().Selectors()[1].String() // inputs: <name>: …
	if name_, err := cueliteral.Unquote(name); err == nil {
		name = name_
	}
	return name
}

func (self InOutCUEString) Output(inputs map[string]Fact) OutputDefinition {
	value := self.valueWithInputs(inputs)
	return OutputDefinition{
		Success: value.LookupPath(cue.MakePath(cue.Str("output"), cue.Str("success"))),
		Failure: value.LookupPath(cue.MakePath(cue.Str("output"), cue.Str("failure"))),
	}
}

func (self InOutCUEString) valueWithInputs(inputs map[string]Fact) cue.Value {
	cueStr := util.CUEString(self)

	if inputs != nil {
		type inputAsDef struct {
			Value any `json:"match"` // ← "match" instead of "value"
			Fact
		}
		inputsAsDef := make(map[string]inputAsDef, len(inputs))
		for k, v := range inputs {
			inputsAsDef[k] = inputAsDef{
				Fact:  v,
				Value: v.Value,
			}
		}

		inputsJson, err := json.Marshal(inputsAsDef)
		if err != nil {
			panic("Facts should always be serializable to JSON")
		}

		cueStr += util.CUEString(
			// XXX check which inputs are actually used and pass in only those
			"\ninputs: {" + string(inputsJson) + "}",
		)
	}

	return cueStr.Value(nil, nil)
}

func (self *RunStatus) FromString(str string) error {
	switch str {
	case "running":
		*self = RunStatusRunning
	case "succeeded":
		*self = RunStatusSucceeded
	case "failed":
		*self = RunStatusFailed
	case "canceled":
		*self = RunStatusCanceled
	default:
		return fmt.Errorf("Unknown RunStatus %s", str)
	}
	return nil
}

func (self RunStatus) String() string {
	switch self {
	case RunStatusRunning:
		return "running"
	case RunStatusSucceeded:
		return "succeeded"
	case RunStatusFailed:
		return "failed"
	case RunStatusCanceled:
		return "canceled"
	default:
		panic(fmt.Sprintf("Unknown RunStatus %d", self))
	}
}

func (self *RunStatus) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}
	return self.FromString(str)
}

func (self RunStatus) MarshalJSON() ([]byte, error) {
	return json.Marshal(self.String())
}

func (self *RunStatus) Scan(value any) error {
	return self.FromString(value.(string))
}

// Sets the value from JSON and returns the rest of the buffer as binary.
func (f *Fact) FromReader(reader io.Reader, trimWhitespace bool) (io.Reader, error) {
	factDecoder := json.NewDecoder(reader)
	if err := factDecoder.Decode(&f.Value); err != nil {
		return nil, errors.WithMessage(err, "Could not unmarshal json body")
	} else {
		binary := io.MultiReader(factDecoder.Buffered(), reader)
		if trimWhitespace {
			binary = util.SkipLeadingWhitespaceReader(binary)
		}
		return binary, nil
	}
}
