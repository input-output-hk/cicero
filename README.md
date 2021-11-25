# Cicero

*Cicero* is a workflow execution engine.  A *workflow* is a description of
(dependent) actions using the *Nix* configuration language. The idea behind
workflows in Cicero is to build reproducible artefacts based on commit hashes
or pull requests. Workflows consist of one more more *Action*. Actions can for
example be source checkouts, build instructions, or quality gates that measure
and ensure certain Key Performance Indicators (KPI) and Key Risk Indicators
(KRI).

*Cicero*’s workflows are flexible enough to build Continuous Integration (CI)
and Continuous Deliver (CD) pipelines. It offers a rich Web UI as well as a CLI
tool for developers to query and inspect workflows and their associated actions,
as well as the action outcomes. Integration with third party applications (e.g.
JIRA) is possible, for automatic status updates. By using a declarative
approach to actions, dependencies and intermediate results can be easily cached,
and execution parallelised, thus reducing build times.

# How To Run

First enter the development shell using either direnv or by running `nix develop`.

Start a development instance of Nomad:

    nomad-dev

Run the required services in Nomad:

    dev-run

Start the Nomad follower to capture logs:

    sudo nomad-follower

Migrate the database:

    dbmate up

Run the application:

    go run . start

Access WebUI:

    open localhost:8080 in browser

# How To …

See the commands listed by:

```
nix develop
```

# Which ports are in use?

```
8080 - Cicero, WebUI
4646 - Nomad, HTTP
4647 - Nomad, RPC
4648 - Nomad, Serf WAN
5432 - PostgreSQL
4222 - Liftbridge, NATS client connections
9292 - Liftbridge, standard client connections
3100 - Loki, HTTP
9095 - Loki, GRPC
```

# Development

Formats Go source code

```
go fmt ./...
```
Run test with coverage

```
go test -cover ./...
```