# Tullia Tutorial

[Tullia](https://github.com/input-output-hk/tullia) provides two things, the [Tullia-CLI](https://github.com/input-output-hk/tullia#cli) to run tasks locally
and the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) to include Cicero Actions in a **flakified** repository.

The term **flakified** means that a project contains a [flake.nix](https://nixos.wiki/wiki/Flakes) file.

Tasks in Tullia are run in isolation by using [nsjail](https://nsjail.dev/).

## Topics
- [Repository layout](./tullia-1.md)
- [Input description explained](./tullia-2.md)
- [Tasks explained](./tullia-3.md)
- [How to create an Action](./tullia-4.md)
- [How to start an Action](./tullia-5.md)
