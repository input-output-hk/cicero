package domain

import (
	"testing"

	"cuelang.org/go/cue"
	"github.com/stretchr/testify/assert"
)

func TestValueWithInputs(t *testing.T) {
	t.Parallel()

	// given
	s := InOutCUEString(`
		inputs: a: match: _
		output: success: inputs.a.match
	`)
	inputs := map[string]Fact{
		"a": {Value: 1},
	}

	// when
	v := s.valueWithInputs(inputs)

	// then
	i, e := v.LookupPath(cue.MakePath(cue.Str("output"), cue.Str("success"))).Int64()
	assert.NoError(t, e)
	assert.Equal(t, int64(1), i)
}
