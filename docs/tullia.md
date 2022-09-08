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
		- [/actions.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix) <- see [input-description section](./tullia.md#input-descriptionactionsnix-explained)
		- [/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) <- see [tasks section](./tullia.md#taskstasksnix-explained)

*In most single project use cases the repository-root and project-root would be the same but in this case the repository holds multiple projects.*

## How to include Tullia-Lib into a repository?

The previous section explained how a repository could be organized to add Cicero Actions.

To be capable to load those actions later on into Cicero it is also required to include the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) into the project-root's [flake.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/flake.nix).

Follow the comments in the provided flake.nix example to get a basic understanding of [nix flakes](https://nixos.wiki/wiki/Flakes) but also how the [Tullia-Lib](https://github.com/input-output-hk/tullia/blob/main/nix/lib.nix) is used.

### Example from [tullia-example/rust/flake.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/flake.nix):
```nix
{
  description = "Flake for Tullia rustExample";

  # inputs describes all the external dependencies
  # to be pulled and used in outputs
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    tullia = {
      url = "github:input-output-hk/tullia";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  # the minimal set to be defined in outputs is defaultPackage,
  # without it 'nix build' won't work
  # devShells is optional but useful if a shell environment
  # should be provided for 'nix develop'
  outputs = { self, nixpkgs, utils, tullia }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {

        # defining defaultPackage as flake output
        # allows 'nix build' to build the derivation
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "rustExample";
          src = ./src;
          buildInputs = with pkgs; [
            coreutils
            gcc
            rustc
          ];
          installPhase = ''
            rustc $src/hello.rs
            mkdir -p $out
            mv ./hello $out
          '';
        };
      } //

      # tullia.fromSimple is a call to the tullia library
      # it creates the cicero & tullia flake outputs
      # which can be evaluated by a Cicero instance
      tullia.fromSimple system {

        # actions & tasks are loaded as sets from the provided nix files
        # Cicero will later on translate those sets into inputs & actions
        actions = import tullia/actions.nix;
        tasks = import tullia/tasks.nix self;
      }
    );
}
```

## Input description(actions.nix) explained

The [input description](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix) is written in [nix](https://nixos.wiki/wiki/Nix_Expression_Language) and [cuelang](https://cuelang.org/).

### Example from [tullia-example/rust/tullia/actions.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix):
```nix
{
  # "rust/build" is the action's name
  "rust/build" = {

    # task, referes to a corresponding task in tullia/tasks.nix
    # this task is executed if the requirements of the input description are fulfilled
    task = "build";

    # io, contains the actual input description in cuelang
    # The module.nix in Tullia merges the cue in io with tullia's cue-libs(lib/*.cue)
    # into the action..io attribute
    # Cicero uses the resulting cue configuration to match & verify incoming inputs(as json)
    # for a specific action
    # module.nix: https://github.com/input-output-hk/tullia/blob/main/nix/module.nix#L994-L1010
    # action..io: https://github.com/input-output-hk/tullia/blob/main/doc/src/module.md#actionio--path
    io = ''

      # github, is a set of repository and input information
      # this set is reused to subscribe to multiple inputs
      let github = {
        #input: "GitHub event"
        #repo: "input-output-hk/tullia-example"
      }

      # lib.merge, ios and lib.io.github*, allow the user
      # to describe inputs & output with less overhead
      # merge & ios: https://github.com/input-output-hk/tullia/blob/main/lib/prelude.cue
      # lib.io.github: https://github.com/input-output-hk/tullia/blob/main/lib/github.cue
      #lib.merge
      #ios: [

        # The input/output entries in ios will tell Cicero to only run the 'build' task
        # if a github_push or github_pr event for the 'tullia-example' repository happens
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
```

## Tasks(tasks.nix) explained

The [tasks](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) are written in [nix](https://nixos.wiki/wiki/Nix_Expression_Language).

### Example from [tullia-example/rust/tullia/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix):
```nix
# rev, is used to point to a specific git commit
# ..., is the self attribute from the flake outputs
{rev ? "HEAD", ...}: let

  # common environment for nix builds
  common = {
    config,
    lib,
    ...
  }: {

    # presets are loaded by the module.nix in tullia
    # and are part of the common environment
    # all presets: https://github.com/input-output-hk/tullia/tree/main/nix/preset
    # documentation: https://github.com/input-output-hk/tullia/blob/main/doc/src/module.md
    preset = {

      # enables nix in this environment
      # preset.nix: https://github.com/input-output-hk/tullia/blob/main/nix/preset/nix.nix
      nix.enable = true;

      # github-ci, wrapper for cloning a repository from github
      # preset.github-ci: https://github.com/input-output-hk/tullia/blob/main/nix/preset/github-ci.nix
      github-ci = __mapAttrs (_: lib.mkDefault) {

        # mkDefault raises priority of each attribute
        enable = config.action.facts != {};
        repo = "input-output-hk/tullia-example";
        sha = config.preset.github-ci.lib.getRevision "GitHub event" rev;
        clone = false;
      };
    };
  };

  # helper function, to determine flakeUrl
  # if no facts available use current directory
  flakeUrl = {
    config,
    lib,
    ...
  }:
    lib.escapeShellArg (
      if config.action.facts != {}
      then "github:${config.preset.github-ci.repo}/${config.preset.github-ci.lib.getRevision "GitHub event" rev}"
      else "."
    );
in {

  # lint task
  lint = {...}: {

    # common environment for nix builds
    imports = [common];

    config = {

      # the actual bash cmd to be executed
      command.text = ''
        nix develop -L -c lint
      '';

      # only this task sets github-ci.clone in it's scope to true
      # to download the github repository only once
      preset.github-ci.clone = true;

      # settings for the nomad scheduler
      memory = 1024 * 2;
      nomad.resources.cpu = 1000;
    };
  };

  # build task
  build = args: {

    # common environment for nix builds
    imports = [common];

    config = {

      # after, will force this task to be run
      # after the "lint" task
      after = ["lint"];

      # the actual bash cmd to be executed
      command.text = "nix build -L ${flakeUrl args}";

      # enable kvm virtualization for this task
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      # settings for the nomad scheduler
      memory = 1024 * 3;
      nomad.resources.cpu = 3500;
    };
  };
}
```

## Creating an Action in the Cicero-WebUI

## Starting an Action in the Cicero-WebUI
