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

Documentation is currently available in the form of source code
[comments](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix), [example actions](https://github.com/input-output-hk/cicero/tree/main/actions/examples) and the [Cicero Handbook](https://cicero-handbook.infra.aws.iohkdev.io/).

# Development

## How To Run

### [Install Nix](https://cicero-handbook.infra.aws.iohkdev.io/install-nix.html) section of the Cicero Handbook.

### [Install Cicero](https://cicero-handbook.infra.aws.iohkdev.io/install-cicero.html) section of the Cicero Handbook.

### There is also an OpenAPI v3 schema available at:
- http://localhost:8000/documentation/cicero.json
- http://localhost:8000/documentation/cicero.yaml

## How To …

Run linters:

	lint

Format all source code:

	treefmt

Build mocks automatically:

	go generate ./...

Run tests with coverage:

	go test -cover ./...

Run OpenApi validation tests:

	schemathesis run http://localhost:8000/documentation/cicero.yaml --validate-schema=false

Serve Cicero book locally on port 3000

	mdbook serve --open

See all commands provided by the development shell:

	menu
