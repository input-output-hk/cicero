# Cicero

Cicero is an action execution engine.

Think of it like an if-this-then-that machine on HashiCorp Nomad.

Cicero’s actions are flexible enough to build Continuous Integration (CI) and
Continuous Delivery (CD) pipelines. It offers a rich Web UI as well as a CLI
tool for developers to query and inspect actions and their runs,
as well as the action output. Integration with third party applications (e.g.
JIRA) is possible, for automatic status updates. By using a declarative
approach to actions, dependencies and intermediate results can be easily cached,
and execution parallelised, thus reducing build times.

## Vocabulary

- An **Action** is a description of a Nomad job with **inputs** and **output**.
	- An **Input** is mainly a CUE document that describes a required **fact**.
	- An **Output** is a JSON document that will be published as a **fact**.
		There may be one output for the success and failure case each.
- **Facts** are arbitrary JSON documents.
	The collection of all facts form Cicero's global state.
	A fact may have binary data attached which is called an **artifact**.
- **Artifacts** are arbitrary binary data attached to a fact.
	There may be none or only one artifact attached to a fact.
	If you want an artifact comprised of multiple files, use an archive format.
- **Runs** are equivalent to a Nomad job spawned by an action.

## Actions

### Lifecycle and Versioning

1. The action is created. This replaces any previous action with the same name.
2. The action is invoked. This may happen any number of times, producing a run.
3. The action is shadowed by a newer action with the same name, see step 1.

Actions that are not shadowed are called the **current** actions.
Shadowed actions with equal names are called the previous versions of an action.

### Invokation

When a fact is published all current actions are checked for runnability.

An action is runnable if its inputs are satisfied. That means there has to be
a matching fact for each input that is not optional and there must be no
matching fact for a negated input. Matching facts must also be different ones
than those that satisfied the input for the previous run of this action.

If an action is runnable it is evaluated with its matching facts given as inputs.
This produces a run for which a job is scheduled on the Nomad cluster.

When this job finishes, respective output are published as new facts,
restarting the cycle.

Facts can also be published from within a run using Cicero's API endpoints
or manually.

# Authoring Actions

Actions can be written in any language that is able to produce JSON.

Evaluation is done by language-specific **evaluators**.
These are simple programs that implement an interface based on CLI arguments
and environment variables by invoking the language's runtime.

Cicero currently only ships a Nix evaluator but others are planned.

## Nix Standard Library

For actions written in Nix, Cicero provides a standard library of functions
that help writing actions' inputs, output, and Nomad jobs in a concise way.

Documentation is currently only available in the form of source code
[comments](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix)
and [example actions](https://github.com/input-output-hk/cicero/tree/main/actions/examples).

# Development

## Prerequisites

- Nix
	- At least version 2.4
	- Enable flakes in your `/etc/nix/nix.conf`: `experimental-features = nix-command flakes`
- (Linux with systemd)
	- You can run Cicero on other platforms but the [Nix actions library](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix) requires [nomad-driver-nix](https://github.com/input-output-hk/nomad-driver-nix).

## How To Run

First enter the development shell using either direnv or by running `nix develop`.

Start a development instance of Nomad, nomad-follower, and Vault:

	dev-cluster

Run the required services in Nomad:

	dev-jobs

Migrate the database:

	dbmate up

Run the application:

	go run . start

Access WebUI:

	open localhost:8080 in browser

Access OpenAPI3 Schema:

	open localhost:8080/documentation/cicero.json in browser // for json
	open localhost:8080/documentation/cicero.yaml in browser // for yaml

## How To …

Run linters:

	lint

Format all source code:

	treefmt

Run tests with coverage:

	go test -cover ./...

Run OpenApi validation tests:

	schemathesis run http://localhost:8080/documentation/cicero.yaml --validate-schema=false

Build mocks automatically:

	go generate ./...

See the commands listed by:

	nix develop

## Which ports are in use?

- 8080: Cicero (HTTP)
- 4646: Nomad (HTTP)
- 4647: Nomad (RPC)
- 4648: Nomad (Serf)
- 5432: PostgreSQL
- 3100: Loki (HTTP)
- 9095: Loki (GRPC)
