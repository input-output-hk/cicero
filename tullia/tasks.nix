{rev ? "HEAD", ...}: let
  common = {
    config,
    lib,
    ...
  }: {
    preset = {
      nix.enable = true;
      github-ci = __mapAttrs (_: lib.mkDefault) {
        enable = config.action.facts != {};
        repo = "input-output-hk/cicero";
        sha = config.preset.github-ci.lib.getRevision "GitHub event" rev;
        clone = false;
      };
    };
  };

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
  lint = {...}: {
    imports = [common];

    config = {
      command.text = ''
        nix develop -L -c lint
      '';

      preset.github-ci.clone = true;

      memory = 1024 * 2;
      nomad.resources.cpu = 1000;
    };
  };

  build = args: {
    imports = [common];

    config = {
      after = ["lint"];

      command.text = "nix build -L ${flakeUrl args}";

      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 3;
      nomad.resources.cpu = 3500;
    };
  };

  handbook = args: {
    imports = [common];

    config = {
      after = ["build"];

      command.text = with common; "
        cue export ./jobs -e jobs.ciceroHandbook \
          ${
          lib.optionalString (cfg ? environment)
          "-t env=${lib.escapeShellArg cfg.environment}"
        } \
          -t sha=${lib.escapeShellArg cfg.sha} \
          > job.json

        nomad run job.json
      ";

      memory = 1024;
      nomad.resources.cpu = 1000;
    };
  };
}
