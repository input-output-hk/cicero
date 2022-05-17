package domain

import (
	"encoding/json"
	"fmt"
	"io"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
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
	Active    bool      `json:"active"`
	ActionDefinition
}

type ActionDefinition struct {
	Meta  map[string]interface{} `json:"meta"`
	InOut InOutDefinition        `json:"io" db:"io"`
}

type InOutDefinition struct {
	Inputs InputDefinitions `json:"inputs"`
	Output OutputDefinition `json:"output"`
}

type InputDefinitions map[string]InputDefinition

type InputDefinition struct {
	Not      bool           `json:"not"`
	Optional bool           `json:"optional"`
	Match    InOutCUEString `json:"match"`
}

type Output struct {
	definition OutputDefinition
	inputs     map[string]*Fact
}

type OutputDefinition struct {
	Success *InOutCUEString `json:"success"`
	Failure *InOutCUEString `json:"failure"`
}

type InOutCUEString util.CUEString

type Invocation struct {
	Id         uuid.UUID `json:"id"`
	ActionId   uuid.UUID `json:"action_id"`
	CreatedAt  time.Time `json:"created_at"`
	EvalStdout *string   `json:"eval_stdout"`
	EvalStderr *string   `json:"eval_stderr"`
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
	ID         uuid.UUID   `json:"id"`
	RunId      *uuid.UUID  `json:"run_id,omitempty"`
	CreatedAt  time.Time   `json:"created_at"`
	Value      interface{} `json:"value"`
	BinaryHash *string     `json:"binary_hash,omitempty"`
	// TODO nyi: unique key over (value, binary_hash)?
}

type NomadEvent struct {
	nomad.Event
	Uid     util.MD5Sum
	Handled bool
}

func (self InOutDefinition) CUEString() (str util.CUEString) {
	var allImports string
	if _, imports, inStr := self.Inputs.CUEString().StripHead(); inStr != "" {
		str += util.CUEString(`inputs: {` + inStr + "}\n")
		allImports += imports + "\n"
	}
	if _, imports, outStr := self.Output.CUEString().StripHead(); outStr != "" {
		str += util.CUEString(`output: {` + outStr + "}\n")
		allImports += imports + "\n"
	}
	str = util.CUEString(allImports) + str
	return
}

func (self *InOutDefinition) UnmarshalJSON(data []byte) error {
	var io util.CUEString
	if err := json.Unmarshal(data, &io); err != nil {
		return err
	}
	return self.FromCUEString(io)
}

func (self *InOutDefinition) FromCUEString(io util.CUEString) error {
	value := io.Value(nil)
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
		self.Output.Success = new(InOutCUEString)
		if err := self.Output.Success.FromValue(v); err != nil {
			return err
		}
	}
	if v := value.LookupPath(pathOutputFailure); v.Exists() {
		self.Output.Failure = new(InOutCUEString)
		if err := self.Output.Failure.FromValue(v); err != nil {
			return err
		}
	}

	return nil
}

func (self *InOutDefinition) Scan(value interface{}) error {
	return self.FromCUEString(util.CUEString(value.(string)))
}

func (self *InputDefinitions) CUEString() (str util.CUEString) {
	var allImports string
	for name, def := range *self {
		str = util.CUEString(cueliteral.Label.Append([]byte(str), name))
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

		_, imports, match := util.CUEString(def.Match).StripHead()
		allImports += imports + "\n"
		str += `match: {` + match + "}\n"

		str += "}\n"
	}
	str = util.CUEString(allImports) + str
	return
}

func (self *InputDefinitions) Flow(runnerFunc flow.RunnerFunc) *flow.Controller {
	util.CueMutex.Lock()
	defer util.CueMutex.Unlock()

	cueStr := ``
	for name, input := range *self {
		_, imports, match := util.CUEString(input.Match).StripHead()
		cueStr += imports + "\n"
		cueStr += `inputs: `
		cueStr = string(cueliteral.Label.Append([]byte(cueStr), name))
		cueStr += `: value: {` + string(match) + "}\n"
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

func (self OutputDefinition) CUEString() (str util.CUEString) {
	var allImports string
	if self.Success != nil {
		_, imports, success := util.CUEString(*self.Success).StripHead()
		str += `success: {` + success + "}\n"
		allImports += imports + "\n"
	}
	if self.Failure != nil {
		_, imports, failure := util.CUEString(*self.Failure).StripHead()
		str += `failure: {` + failure + "}\n"
		allImports += imports + "\n"
	}
	str = util.CUEString(allImports) + str
	return
}

func (self OutputDefinition) WithInputs(inputs map[string]*Fact) Output {
	return Output{self, inputs}
}

func (self Output) Success() *cue.Value {
	if self.definition.Success == nil {
		return nil
	}

	v := self.definition.Success.ValueWithInputs(self.inputs)
	return &v
}

func (self Output) Failure() *cue.Value {
	if self.definition.Failure == nil {
		return nil
	}

	v := self.definition.Failure.ValueWithInputs(self.inputs)
	return &v
}

func (self Output) MarshalJSON() ([]byte, error) {
	result := map[string]*cue.Value{}
	if success := self.Success(); success != nil {
		result["success"] = success
	}
	if failure := self.Failure(); failure != nil {
		result["failure"] = failure
	}
	return json.Marshal(result)
}

func (self InOutCUEString) ValueWithInputs(inputs map[string]*Fact) cue.Value {
	return util.CUEString(self).Value(func(ctx *cue.Context) []cue.BuildOption {
		return []cue.BuildOption{cue.Scope(ctx.Encode(struct {
			Inputs map[string]*Fact `json:"inputs"`
		}{
			// XXX check which inputs are actually used and pass in only those
			inputs,
		}))}
	})
}

func (self *InOutCUEString) FromValue(v cue.Value) error {
	return (*util.CUEString)(self).FromValue(v)
}

func (self *InOutCUEString) UnmarshalJSON(data []byte) error {
	var str util.CUEString
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}

	// Fail to unmarshal an error value to fail early.
	if err := str.Value(nil).Err(); err != nil {
		return err
	}

	*self = InOutCUEString(str)
	return nil
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

func (self *RunStatus) Scan(value interface{}) error {
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
