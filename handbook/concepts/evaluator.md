# Evaluators

Evaluators are responsible for reading actions from downloaded [sources](source.md).
They are started by an [invocation](invocation.md) or when [registering](action.md#registration) an action.

The actual files and their contents in a source are the evaluator's concern.
It is what defines the format for these files.

At the moment only a [Nix evaluator](evaluators/nix.md) is implemented that reads actions from `.nix` files.
We would like to implement other evaluators in the future.

## Protocol

Cicero communicates with evaluators over a very rudimentary protocol
consisting of environment variables, command line arguments, and stdout.

### Environment Variables

| Name                   | Description                                                                                                                                                                                           |
|------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `CICERO_ACTION_ID`     | The UUID of the [action](action.md) being evaluated.                                                                                                                                                  |
| `CICERO_ACTION_NAME`   | The name of the action being evaluated.                                                                                                                                                               |
| `CICERO_ACTION_INPUTS` | Path to a file containing a JSON object that contains the action's inputs except for missing optional or negated ones. The key is the input name and the value is the entire fact (not just its value) except for the artifact. This is only set when evaluating for a [run](run.md). |

### Command Line Arguments

The first argument (position 1) is either `list` or `eval`:

- `list` means that Cicero expects an array of strings in the [`result`](#result) event on stdout.
- `eval` means that Cicero expects an object with one key for each attribute named by the remaining arguments.
	These may be one of [`meta`](action.md#meta), [`io`](action.md#io), or [`job`](action.md#job).

### stdout

An evaluator prints [newline-delimited JSON](https://jsonlines.org) messages to reply to Cicero.

Each message is an object that has an `event` key according to which the rest of the message
is to be interpreted. The formats for different event types follow.

#### `result`

| Field Name | Description                                                                                                |
|------------|------------------------------------------------------------------------------------------------------------|
| `event`    | `result`                                                                                                   |
| `result`   | The JSON value expected by Cicero according to the first [command line argument](#command-line-arguments). |

#### `error`

| Field Name | Description       |
|------------|-------------------|
| `event`    | `error`           |
| `error`    | An error message. |
