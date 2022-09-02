# Tullia Tutorial

[Tullia](https://github.com/input-output-hk/tullia) provides two things, the [Tullia-CLI](https://github.com/input-output-hk/tullia#cli) to run tasks locally
and the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) to include Cicero Actions in a **flakified** repository.

The term **flakified** means that a project contains a [flake.nix](https://nixos.wiki/wiki/Flakes) file.

Tasks in Tullia are run in isolation by using [nsjail](https://nsjail.dev/).

## Repository layout

The most significant change is that the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) loads the input description for actions and the actually performed tasks from seperate files.

According to the [tullia-example](https://github.com/input-output-hk/tullia-example) repository lay out the following:
- [tullia-example/](https://github.com/input-output-hk/tullia-example) <- repository-root
  - [/rust/](https://github.com/input-output-hk/tullia-example/tree/main/rust) <- an actual project-root with a flake.nix
	- [/tullia/](https://github.com/input-output-hk/tullia-example/tree/main/rust/tullia) <- here lies the tullia code
		- [/actions.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix) <- input description, [nix](https://nixos.wiki/wiki/Nix_Expression_Language) + [cuelang](https://cuelang.org/)
		- [/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) <- tasks, [nix](https://nixos.wiki/wiki/Nix_Expression_Language)

*In most single project use cases the repository-root and project-root would be the same but in this case the repository holds multiple projects.*

## How to include Tullia-Lib into a repository?

The previous section explained how a repository could be organized to add Cicero Actions.

To be capable to load those actions later on into Cicero it is also required to include the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) into the project-root's flake.nix.

### Example from [tullia-example/rust/flake.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/flake.nix):
```
TODO
```

## Input description(actions.nix) explained

## Tasks(tasks.nix) explained

## Creating an Action in the Cicero-WebUI

## Starting an Action in the Cicero-WebUI
