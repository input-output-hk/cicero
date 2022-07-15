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

      memory = 1024 * 2;
      nomad.resources.cpu = 1000;
    };
  };

  schemathesis = args: {
    imports = [common];

    config = {
      after = ["build"];

      command.text = ''
        system=$(nix eval --raw --impure --expr __currentSystem)
        nix build -L ${flakeUrl args}#checks."$system".schemathesis
      '';

      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 3;
      nomad.resources.cpu = 3000;
    };
  };
}
