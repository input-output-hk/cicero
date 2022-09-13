# Tasks(tasks.nix) explained

The [tasks](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) are written in [nix](https://nixos.wiki/wiki/Nix_Expression_Language).

## Example from [tullia-example/rust/tullia/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix):
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
