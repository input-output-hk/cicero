{rev ? "HEAD", ...}: let
  common = {
    config,
    lib,
    ...
  }: {
    preset = {
      nix.enable = true;
      github.ci = __mapAttrs (_: lib.mkDefault) {
        enable = config.actionRun.facts != {};
        repository = "input-output-hk/cicero";
        revision = config.preset.github.lib.readRevision "GitHub event" rev;
      };
    };
  };
in {
  lint = {...}: {
    imports = [common];

    config = {
      command.text = ''
        nix develop -L -c lint
      '';

      memory = 1024 * 2;
      nomad.resources.cpu = 1000;
    };
  };

  build = {...}: {
    imports = [common];

    config = {
      after = ["lint"];

      command.text = "nix build -L";

      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 3;
      nomad.resources.cpu = 3500;
    };
  };
}
