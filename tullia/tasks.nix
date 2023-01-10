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

    nomad.driver = "exec";
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

  build = {config, ...}: {
    imports = [common];

    config = {
      after = ["lint"];

      command.text = config.preset.github.status.lib.reportBulk {
        bulk.text = "nix eval .#defaultPackage --apply __attrNames --json | nix-systems -i";
        each.text = ''nix build -L .#defaultPackage."$1"'';
      };

      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 3;
      nomad.resources.cpu = 3500;
    };
  };
}
