# Transformers

Transformers are run as part of [invocation](invocation.md).

Each transformer receives the [job](action.md#job) on stdin, modifies it, and prints it on stdout.

They are called in the order they appear in in Cicero's configuration,
which means each transformer receives the result of the previous.

Transformers are needed to make changes to jobs that could not be made in the action source itself,
such as setting security or scheduling constraints that are specific to your instance of the scheduler (Nomad).
