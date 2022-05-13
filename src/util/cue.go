package util

import (
	"sync"
	"regexp"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	cueformat "cuelang.org/go/cue/format"
)

// There is a race condition around global internal state of CUE.
var CueMutex = &sync.Mutex{}

type CUEString string

// Parses into a NEW CONTEXT!
func (self CUEString) Value(optionsFunc func(*cue.Context) []cue.BuildOption) cue.Value {
	CueMutex.Lock()
	defer CueMutex.Unlock()

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

func (self CUEString) StripHead() (pkg string, imports string, rest CUEString) {
	rest = self

	if i := regexp.MustCompile(`\s*(package\s+\w+\r?\n)?\s*`).FindIndex([]byte(rest)); i != nil {
		pkg = string(rest[i[0]:i[1]])
		rest = CUEString(rest[i[1]:])
	}

	if i := regexp.MustCompile(`\s*(import\s+(\"\w+\"\r?\n|\([^\)]+\))\s*)*`).FindIndex([]byte(rest)); i != nil {
		imports = string(rest[i[0]:i[1]])
		rest = CUEString(rest[i[1]:])
	}

	return
}
