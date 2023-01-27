# Tasks(tasks.nix) explained

The [tasks](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix) are written in [nix](https://nixos.wiki/wiki/Nix_Expression_Language).

## Example from [tullia-example/rust/tullia/tasks.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/tasks.nix):
```nix
let
  # common environment for nix builds
  common = {
    config,
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
      github-ci = {
        enable = config.actionRun.facts != {};
        repo = "input-output-hk/tullia-example";
        sha = config.preset.github-ci.lib.getRevision "GitHub event" rev;
      };
    };

    # nomad templates are a convenient way to ship configuration files within a nomad task
    # https://www.nomadproject.io/docs/job-specification/template
    # In this example a netrc file is created to enable authentication against github.com
    nomad.templates = [
      {
        destination = "${config.env.HOME}/.netrc";
        data = ''
          machine github.com
          login git
          password {{with secret "kv/data/cicero/github"}}{{.Data.data.token}}{{end}}

          machine api.github.com
          login git
          password {{with secret "kv/data/cicero/github"}}{{.Data.data.token}}{{end}}
        '';
      }
    ];
  };

in {

  # lint task
  lint = {...}: {

    # common environment for nix builds
    imports = [common];

    # the actual bash cmd to be executed
    command.text = ''
      nix develop -L ./rust -c clippy-driver rust/src/hello.rs
    '';

    # settings for the nomad scheduler
    memory = 1024 * 2;
    nomad.resources.cpu = 1000;
  };

  # build task
  build = args: {

    # common environment for nix builds
    imports = [common];

    # after, will force this task to be run
    # after the "lint" task
    after = ["lint"];

    # the actual bash cmd to be executed
    command.text = ''
      nix build -L ./rust
    '';

    # settings for the nomad scheduler
    memory = 1024 * 3;
    nomad.resources.cpu = 3500;
  };
}
```
