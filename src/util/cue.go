package util

import (
	"strings"
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
		self.removeDestructiveBottomComments()
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
	result.removeDestructiveBottomComments()

	return
}

// CUE inserts a comment after every bottom literal
// but unfortunately has a few edge cases in which
// it does not properly add a newline after it, or
// when it does breaks up the expression that the
// bottom literal was in, thereby destroying syntax.
// TODO file upstream bug
func (self *CUEString) removeDestructiveBottomComments() {
	*self = CUEString(strings.ReplaceAll(string(*self), "// explicit error (_|_ literal) in source\n", ""))
}

// https://github.com/cue-lang/cue/issues/883
func IsConcreteRecursive(value cue.Value) bool {
	concrete := true
	def, _ := value.Default()
	def.Walk(func(v cue.Value) bool {
		switch k := v.Kind(); {
		case k.IsAnyOf(cue.StructKind | cue.ListKind):
			return true
		case k == cue.BottomKind:
			switch op, vals := v.Expr(); op {
			case cue.OrOp:
				for _, val := range vals {
					if IsConcreteRecursive(val) {
						concrete = true
						break
					}
				}
			case cue.AndOp:
				for _, val := range vals {
					if !IsConcreteRecursive(val) {
						concrete = false
						break
					}
				}
			default:
				concrete = v.IsConcrete()
			}
		default:
			concrete = v.IsConcrete()
		}
		return concrete
	}, nil)
	return concrete
}
