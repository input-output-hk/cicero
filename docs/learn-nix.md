# Learn Nix

## Nix disclaimer
To actually write Cicero Actions it is required to know the [Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language).

The same link provides good ressources on how to learn the language.

To get a quick overview it is advised to follow the [Interactive Tour](https://nixcloud.io/tour/?id=1).

The language itself, is basically a DSL for packaging software.

In combination with the [Nix package manager](https://nixos.org/manual/nix/stable/), it leverages the possibilties to declaratively manage and install a system through only nix expressions.

Also having a basic knowledge how [Flakes](https://nixos.wiki/wiki/Flakes) in NixOS work, will help to understand the [flake.nix](https://github.com/input-output-hk/cicero/blob/main/flake.nix) in the Cicero repository.

## Cicero flake.nix
The [Cicero Infrastructure Components](https://miro.com/app/board/uXjVOBqekRU=/) diagram shows what happens in the different stages of the Cicero [flake.nix](https://github.com/input-output-hk/cicero/blob/main/flake.nix).

Those are the same dev-{cluster, jobs, cicero} steps from the [Install Cicero](./install-cicero.md) part of this handbook.

In the bottom part of the diagram, all the external Actors are listed and how they interact with Cicero.

Currently the local installment of Cicero has a WebUI and API available on [http://localhost:8080](http://localhost:8080).

The actual OpenAPI v3 schema of the API can viewed at:
- [http://localhost:8080/documentation/cicero.json](http://localhost:8080/documentation/cicero.json)
- [http://localhost:8080/documentation/cicero.yaml](http://localhost:8080/documentation/cicero.yaml)

## Cicero + Nix Expression Language

In case of Cicero the Nix Expression Language was extended to make it possible to express executable Actions in it.

Those Action expressions can be used like functions, which are syntactically evaluated by Cicero.

When provided with a corresponding Input, which we will call Fact from now on, an Action can be started.

Therefore a Fact acts as variable input to a function, which is called Action in Cicero.

The [Evaluation and Execution of Cicero Actions](./evaluation-and-execution-of-actions.md) explains this in more depth.
