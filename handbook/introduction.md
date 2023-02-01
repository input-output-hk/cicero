# Introduction

Cicero is an action execution engine that can be used for a variety of tasks.

However, one primary use case is Continuous Integration (CI) and Continuous Delivery (CD),
which this handbook focuses on.

Compared to other CI/CD tools like [Jenkins](https://www.jenkins.io/) or [Hydra](https://github.com/NixOS/hydra),
Cicero is more flexible in its definition of triggering conditions or pipelines as well as tasks that it can run.

Cicero has a rudimentary web UI that can be used to create actions and inspect runs and facts.

There is no dedicated CLI yet. Instead you can talk to the HTTP API using a tool like [httpie](https://httpie.io/) or curl.
Cicero serves an OpenAPI v3 [specification](https://cicero.ci.iog.io/documentation/cicero.yaml) that you can comfortably explore on Swagger UI.

See the [README](https://github.com/input-output-hk/cicero/blob/main/README.md) for an overview of the concepts and vocabulary used.
