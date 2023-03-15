# Inputs

Every [action](action.md) has at least one and possibly more inputs.

They are responsible for specifying when the action is to be [invoked](invocation.md).

## Format

Inputs are written in [CUE](https://cuelang.org) as part of [`io`](action.md#io) and comprise of three fields.

### `match`

The CUE value of this field is used match against facts' values.

Must be a `struct`.

### `optional`

If `true`, this input does not need to be satisfied in order to invoke the action.
It will be invoked when all other non-optional inputs are satisfied.

Must be a `bool`.

### `not`

If `true`, satisfaction of this input prevents the action from being invoked.
The action can only be invoked when this input is not satisfied.

Such inputs are also referred to as "negated".

Must be a `bool`.

## Dependencies

Inputs may depend on each other. Cycles are not allowed.

This can be achieved by referring to the `match` field of another input.

## Examples

```cue
inputs: {
	// An input called "A" that requires a string field "foo" to be present.
	A: match: foo: string

	// A negated input called "B" that requires a struct field "bar" to be present.
	B: {
		not: true
		match: bar: {...}
	}

	// An optional input called "C" that requires an int field "baz" to be present.
	C: {
		optional: true
		match: baz: int
	}

	// An input called "D" that depends on both A and B
	// and requires the field "foo" to match A's field "foo"
	// and requires the field "bar" to match B's field "bar".
	D: match: {
		foo: inputs.A.match.foo
		bar: inputs.B.match.bar
	}
}
```
