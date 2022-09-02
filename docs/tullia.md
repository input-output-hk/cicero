# Tullia

[Tullia](https://github.com/input-output-hk/tullia) provides another abstraction for writing Cicero Actions.

## Repository layout

The most significant change is that Tullia puts the input description for actions and the actually performed tasks in seperate files.

According to the [tullia-example](https://github.com/input-output-hk/tullia-example) repository the layout is the following:
- [tullia-example/](https://github.com/input-output-hk/tullia-example) <- repository-root
  - [/rust/](https://github.com/input-output-hk/tullia-example/tree/main/rust) <- an actual project-root with a flake.nix
	- [/tullia/](https://github.com/input-output-hk/tullia-example/tree/main/rust/tullia) <- here lies the tullia code
		- [/actions.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix) <- input description, [nix](https://nixos.wiki/wiki/Nix_Expression_Language) + [cuelang](https://cuelang.org/)
		- [/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) <- tasks, [nix](https://nixos.wiki/wiki/Nix_Expression_Language)

In most single project use cases the repository-root and project-root would be the same but in this case the repository holds multiple projects.
