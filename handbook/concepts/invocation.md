# Invocations

Before Cicero can schedule an [action](action.md)'s job it needs to invoke it.

That means it runs the evaluator, passing it the satisfied [inputs](input.md).
As a result it gets back the [job](action.md#job) which is then passed on
to all configured [transformers](transformer.md) before a [run](run.md) is started.
