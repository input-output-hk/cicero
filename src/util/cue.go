package util

import (
	"sync"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	cueformat "cuelang.org/go/cue/format"
)

// There is a race condition around global internal state of CUE.
var cueMutex = &sync.Mutex{}

type CUEString string

func (self CUEString) Value(ctx *cue.Context, optionsFunc func(*cue.Context) []cue.BuildOption) cue.Value {
	if ctx == nil {
		cueMutex.Lock()
		defer cueMutex.Unlock()

		ctx = cuecontext.New()
	}

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
	value := self.Value(nil, nil)
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
