# Output

An [action](action.md) may have an output.

This specifies a [fact](fact.md) to publish when the [run](run.md) ends.

## Format

An output is written in [CUE](https://cuelang.org) as part of [`io`](action.md#io) and may have two fields.

These must be concrete values so that they can be converted to JSON.
That means type or value bounds such as `string` or `>10` are not allowed.
A run will not start if this is violated.

An output may refer to [inputs](input.md).

### `success`

This is the value of the fact to publish in case the run succeeded.

No fact will be published if this is `null` or missing.

### `failure`

This is the value of the fact to publish in case the run failed.

No fact will be published if this is `null` or missing.

## Example

```cue
output: {
	// A fact with an `ok` field set to `true` will be published if the run succeeds.
	success: ok: true

	// No fact will be published if the run fails.

	// In both cases (only success in this example),
	// the field `foo` in the fact value will be set to the `foo` field from input A.
	[string]: foo: inputs.A.match.foo
}
```
