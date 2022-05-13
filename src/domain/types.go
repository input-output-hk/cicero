package domain

import (
	"encoding/json"
	"fmt"
	"io"
	"sync"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	cueformat "cuelang.org/go/cue/format"
	cueliteral "cuelang.org/go/cue/literal"
	"cuelang.org/go/tools/flow"
	"github.com/google/uuid"
	nomad "github.com/hashicorp/nomad/api"
	"github.com/pkg/errors"

	"github.com/input-output-hk/cicero/src/util"
)

// There is a race condition around global internal state of CUE.
var cueMutex = &sync.Mutex{}

type CUEString string

// Parses into a NEW CONTEXT!
func (self CUEString) Value(optionsFunc func(*cue.Context) []cue.BuildOption) cue.Value {
	cueMutex.Lock()
	defer cueMutex.Unlock()

	ctx := cuecontext.New()

	var options []cue.BuildOption
	if optionsFunc == nil {
		options = []cue.BuildOption{}
	} else {
		options = optionsFunc(ctx)
	}

	return ctx.CompileString(string(self), options...)
}

func (self *CUEString) FromValue(value cue.Value, options ...cueformat.Option) error {
	if syntax, err := cueformat.Node(
		value.Syntax(
			cue.Hidden(true),
			cue.Optional(true),
			cue.ResolveReferences(false),
		),
		options...,
	); err != nil {
		return err
	} else {
		*self = CUEString(syntax)
	}
	return nil
}

// Converts to a `cue.Value` and back to string so that the caller can use `cueformat.Simplify()`.
// If that is not needed use `cueformat.Source()` directly instead.
func (self CUEString) Format(options ...cueformat.Option) (result CUEString, err error) {
	value := self.Value(nil)
	err = result.FromValue(value, options...)
	if err != nil {
		return
	}
	err = value.Err()
	if err != nil {
		return
	}

	var b []byte
	b, err = cueformat.Source([]byte(result), options...)
	result = CUEString(b)

	return
}

type InOutString CUEString

func (self InOutString) ValueWithInputs(inputs map[string]*Fact) cue.Value {
	return CUEString(self).Value(func(ctx *cue.Context) []cue.BuildOption {
		return []cue.BuildOption{cue.Scope(ctx.Encode(struct{}{}).
			// XXX check which inputs are actually used and pass in only those
			FillPath(cue.MakePath(cue.Str("inputs")), inputs),
		)}
	})
}

func (self *InOutString) FromValue(v cue.Value) error {
	str := new(CUEString)
	if err := str.FromValue(v); err != nil {
		return err
	}
	*self = InOutString(*str)
	return nil
}

func (self *InOutString) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}

	match := CUEString(str)

	// Fail to unmarshal an error value to fail early.
	if err := match.Value(nil).Err(); err != nil {
		return err
	}

	*self = InOutString(match)
	return nil
}

type InOutDefinition struct {
	Inputs InputDefinitions `json:"inputs"`
	Output OutputDefinition `json:"output"`
}

func (self InOutDefinition) CUEString() (str CUEString) {
	if inStr := self.Inputs.CUEString(); inStr != "" {
		str += CUEString(`inputs: {` + inStr + "}\n")
	}
	if outStr := self.Output.CUEString(); outStr != "" {
		str += CUEString(`output: {` + outStr + "}\n")
	}
	return
}

func (self *InOutDefinition) UnmarshalJSON(data []byte) error {
	var io InOutString
	if err := io.UnmarshalJSON(data); err != nil {
		return err
	}
	return self.FromInOutString(io)
}

func (self *InOutDefinition) FromInOutString(io InOutString) error {
	value := CUEString(io).Value(nil)
	if err := value.Err(); err != nil {
		return err
	}

	pathInputs := cue.MakePath(cue.Str("inputs"))
	pathOutputSuccess := cue.MakePath(cue.Str("output"), cue.Str("success"))
	pathOutputFailure := cue.MakePath(cue.Str("output"), cue.Str("failure"))

	self.Inputs = InputDefinitions{}
	if fields, err := value.LookupPath(pathInputs).Fields(); err != nil {
		return err
	} else {
		pathNot := cue.MakePath(cue.Str("not"))
		pathOptional := cue.MakePath(cue.Str("optional"))
		pathMatch := cue.MakePath(cue.Str("match"))

		for fields.Next() {
			field := fields.Value()

			def := InputDefinition{}

			if v := field.LookupPath(pathNot); v.Exists() {
				if not, err := v.Bool(); err != nil {
					return err
				} else {
					def.Not = not
				}
			} else {
				def.Not = false
			}

			if v := field.LookupPath(pathOptional); v.Exists() {
				if optional, err := v.Bool(); err != nil {
					return err
				} else {
					def.Optional = optional
				}
			} else {
				def.Optional = false
			}

			if v := field.LookupPath(pathMatch); !v.Exists() {
				return fmt.Errorf(`input %q must have a "match" field`, fields.Label())
			} else if err := def.Match.FromValue(v); err != nil {
				return err
			}

			self.Inputs[fields.Label()] = def
		}
	}

	if v := value.LookupPath(pathOutputSuccess); v.Exists() {
		var success InOutString
		self.Output.Success = &success
		if err := self.Output.Success.FromValue(v); err != nil {
			return err
		}
	}
	if v := value.LookupPath(pathOutputFailure); v.Exists() {
		var failure InOutString
		self.Output.Failure = &failure
		if err := self.Output.Failure.FromValue(v); err != nil {
			return err
		}
	}

	return nil
}

func (self *InOutDefinition) Scan(value interface{}) error {
	return self.FromInOutString(InOutString(value.(string)))
}

type InputDefinition struct {
	Not      bool        `json:"not"`
	Optional bool        `json:"optional"`
	Match    InOutString `json:"match"`
}

type InputDefinitions map[string]InputDefinition

func (self *InputDefinitions) CUEString() (str CUEString) {
	for name, def := range *self {
		str = CUEString(cueliteral.Label.Append([]byte(str), name))
		str += `: {`

		str += `not: `
		if def.Not {
			str += `true`
		} else {
			str += `false`
		}
		str += "\n"

		str += `optional: `
		if def.Optional {
			str += `true`
		} else {
			str += `false`
		}
		str += "\n"

		str += `match: {` + CUEString(def.Match) + "}\n"

		str += "}\n"
	}
	return
}

func (self *InputDefinitions) Flow(runnerFunc flow.RunnerFunc) *flow.Controller {
	cueMutex.Lock()
	defer cueMutex.Unlock()

	cueStr := ``
	for name, input := range *self {
		cueStr += `inputs: `
		cueStr = string(cueliteral.Label.Append([]byte(cueStr), name))
		cueStr += `: value: {` + string(input.Match) + "}\n"
	}
	value := cuecontext.New().CompileString(cueStr)

	return flow.New(
		&flow.Config{Root: cue.MakePath(cue.Str("inputs"))},
		value,
		func(v cue.Value) (flow.Runner, error) {
			if len(v.Path().Selectors()) != 2 {
				return nil, nil
			}
			return runnerFunc, nil
		},
	)
}

type OutputDefinition struct {
	Success *InOutString `json:"success"`
	Failure *InOutString `json:"failure"`
}

func (self OutputDefinition) CUEString() (str CUEString) {
	if self.Success != nil {
		str += "success: " + CUEString(*self.Success) + "\n"
	}
	if self.Failure != nil {
		str += "failure: " + CUEString(*self.Failure) + "\n"
	}
	return
}

func (self OutputDefinition) WithInputs(inputs map[string]*Fact) Output {
	return Output{self, inputs}
}

type Output struct {
	definition OutputDefinition
	inputs     map[string]*Fact
}

func (self Output) Success() (*string, error) {
	if self.definition.Success == nil {
		return nil, nil
	}

	if b, err := self.definition.Success.ValueWithInputs(self.inputs).MarshalJSON(); err != nil {
		return nil, err
	} else {
		str := string(b)
		return &str, nil
	}
}

func (self Output) Failure() (*string, error) {
	if self.definition.Failure == nil {
		return nil, nil
	}

	if b, err := self.definition.Failure.ValueWithInputs(self.inputs).MarshalJSON(); err != nil {
		return nil, err
	} else {
		str := string(b)
		return &str, nil
	}
}

type ActionDefinition struct {
	Meta  map[string]interface{} `json:"meta"`
	InOut InOutDefinition        `json:"io" db:"io"`
}

type Fact struct {
	ID         uuid.UUID   `json:"id"`
	RunId      *uuid.UUID  `json:"run_id,omitempty"`
	CreatedAt  time.Time   `json:"created_at"`
	Value      interface{} `json:"value"`
	BinaryHash *string     `json:"binary_hash,omitempty"`
	// TODO nyi: unique key over (value, binary_hash)?
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

type Action struct {
	ID        uuid.UUID `json:"id"`
	Name      string    `json:"name"`
	Source    string    `json:"source"`
	CreatedAt time.Time `json:"created_at"`
	Active    bool      `json:"active"`
	ActionDefinition
}

type Run struct {
	NomadJobID   uuid.UUID  `json:"nomad_job_id"`
	InvocationId uuid.UUID  `json:"invocation_id"`
	CreatedAt    time.Time  `json:"created_at"`
	FinishedAt   *time.Time `json:"finished_at"`
}

type Invocation struct {
	Id         uuid.UUID `json:"id"`
	ActionId   uuid.UUID `json:"action_id"`
	CreatedAt  time.Time `json:"created_at"`
	EvalStdout *string   `json:"eval_stdout"`
	EvalStderr *string   `json:"eval_stderr"`
}

type NomadEvent struct {
	nomad.Event
	Uid     MD5Sum
	Handled bool
}

type MD5Sum [16]byte

func (self *MD5Sum) Scan(value interface{}) error {
	if b, ok := value.([]byte); !ok {
		return fmt.Errorf("Cannot scan %T into MD5Sum", value)
	} else if copied := copy(self[:], b); copied != len(*self) {
		return fmt.Errorf("Could only copy %d/%d bytes into MD5Sum", copied, len(*self))
	}
	return nil
}
