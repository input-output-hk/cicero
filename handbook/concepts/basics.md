# How It Works

Let's get a quick overview over an action's lifecycle.

The vocabulary is explained in more detail in the following sections.

## Registration

An [action](concepts/action.md) begins its life by being registered (or created, whatever word you prefer).
That means someone enters its source location and name in the web UI or submits it over the API.

Any given [source](concepts/source.md) can contain one or multiple actions.

Cicero does not know about this directly. It just downloads the source to a local cache
and runs one of the configured [evaluators](concepts/evaluator.md) in it.

The evaluator then tells Cicero which actions actually exist in that source.
This means Cicero makes absolutely no assumptions about the files that may or may not exist in a given source.

## Satisfaction

Every action declares one or multiple [inputs](concepts/input.md),
which simply put is a pattern that a [fact](concepts/fact.md) must match to satisfy the input.

Cicero waits for facts to be posted to its API. When that happens, it will match the fact
against every input of every action and remember which inputs it satisfies.

Once all inputs of a particular action are satisfied, it will be invoked.

## Invocation

Cicero runs the evaluator that is responsible for the action's source
and passes it the facts that matched each of the action's inputs
to obtain information about how to run the action.

## Run

Finally Cicero schedules a job on the scheduler (only Nomad is currently supported)
and waits until that job either succeeds or fails.

## Output

When the job is done Cicero publishes the action's [output](concepts/output.md)
as a new fact. This in turn may start other actions; the cycle begins anew.

## Deactivation

A user may deactivate an action at any time.
This will simply prevent it from being invoked
and hide it from the web UI.

There is currently no way to delete an action other than dropping it directly from the database.

We want to keep a permanent record of everything to be able to
answer questions such as what happened when, triggered by whom, and why,
as the API endpoint `/run?input=<UUID>&recursive` does.
