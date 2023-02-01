# Sources

A source is a location that may contain one or multiple [actions](action.md).

Most commonly these are Git repositories but can also be arbitrary places on the internet or your local disk.

## Format

Sources are [go-getter URLs](https://github.com/hashicorp/go-getter/blob/main/README.md#url-format). See their documentation for details.

A source may also specify which [evaluator](evaluator.md) must be used with it by including its name as the fragment of the URL (the part after `#`).

If it does not specify a particular evaluator, Cicero will try all evaluators and use the first one that succeeds.

Although not common practice a source may contain multiple actions for different evaluators
so if you do not specify an evaluator name in the fragment, the first succeeding one in Cicero's configuration will be used.
This may change if the configuration is changed. It is therefore not advisable to leave off the fragment in this case.
Also you cannot specify multiple evaluators in the URL fragment; two separate sources, one for each evaluator, would be necessary.

A source cannot specify a particular action's name; it always refers to the location of possibly multiple actions.
To really know the location of an action, the action's name is needed in addition to a source that includes the evaluator to use.

## Example

This is the source of Cicero's own CI action:

	github.com/input-output-hk/cicero#nix

## Fetching

If you do not specify an immutable URL (for example including a git revision)
it will, as you may expect, always refer to the latest version available at that URL.

This means Cicero will always download a fresh copy every time it wants to evaluate the source.

Note that action [inputs](input.md) and [output](output.md) are saved in Cicero's database
upon action registration time. They will **not** update to the latest version!
It is therefore necessary to recreate an action if you changed its inputs or output.

## Caching

When Cicero needs to evaluate a source it first downloads it to a cache on the local disk
at `$XDG_CACHE_HOME/cicero/sources` into a directory named by the base64-encoded source URL.

Unless an evaluation is currently running it is safe to delete these.
Cicero will simply fetch them again next time.
