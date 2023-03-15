# Actions

Actions are the primary actor on Cicero. Without them, nothing happens, just like [facts](fact.md).

Actions contain the job to schedule to do the actual work you want to do.
They may also not contain any job at all, in which case the action is called a [decision](#decision).

An action is [invoked](invocation.md) when all its [inputs](input.md) are satisfied by a matching fact.

Where an action is to be found is specified by the [source](source.md).

The lifecycle of an action is described [here](basics.md).

## Decision

A decision is simply a fact that does not declare any job to schedule.

Cicero will just publish the output fact directly upon invocation.

This can be used to transform facts into new ones
or to wait on a number of facts as a synchronisation mechanism.

## Versioning

There can only be one [active](basics.md#deactivation) action for a given name at a time.

When an action with the same name as an existing one is registered
it shadows the existing one. This means it will no longer be invoked.

We say the newly registered action becomes the current version
and the shadowed action becomes a previous version for that name.

## Definition

An action is evaluated by its source's [evaluator](evaluator.md)
so the precise file format is determined by the evaluator used.

Conceptually though every action consists of the same parts.

### `meta`

An action may have arbitrary data attached to it in the form of a JSON object.

This is not used for anything except showing it in the web UI.

### `io`

The `io` [CUE](https://cuelang.org) expression declares both inputs and output.

Cicero will save it to its database to match incoming facts against the inputs and to evaluate the output.

`io` is a `struct` with two fields: [`inputs`](input.md) and [`output`](output.md).

### `job`

The `job` is simply a JSON value passed to the scheduler.
Cicero makes no assumptions about this, although certain [transformers](transformer.md) might.

If this is `null` or missing the action is called a [decision](#decision).
