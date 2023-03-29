# Cicero

Cicero is an action execution engine.

Think of it like an if-this-then-that machine on HashiCorp Nomad.

It is primarily used to implement CI/CD but just as suitable for other automation tasks.

Documentation can be found in the [handbook](https://handbook.cicero.ci.iog.io).

## Tullia

[Tullia](https://github.com/input-output-hk/tullia) provides a library
for writing actions in the Nix expression language.

## Development

See the [_Running Cicero_](https://handbook.cicero.ci.iog.io/setup/run-cicero.html) chapter of the handbook.

### How To …

Run linters:

	lint

Format all source code:

	nix fmt

Build mocks automatically:

	go generate ./...

Run tests with coverage:

	go test -cover ./...

Serve the handbook locally on port 3000 and open it:

	mdbook serve --open

See all commands provided by the development shell:

	menu
