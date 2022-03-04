{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args: {
  inputs.start = ''
    "cicero/ci": start: {
      clone_url: string
      sha: string
      statuses_url?: string
    }
  '';

  job = {start}: let
    cfg = start.value."cicero/ci".start;
  in
    std.chain args [
      actionLib.simpleJob

      (std.networking.addNameservers ["1.1.1.1"])

      (lib.optionalAttrs (cfg ? statuses_url)
        (std.github.reportStatus cfg.statuses_url))

      (std.git.clone cfg)

      {
        resources.memory = 1024 * 3;

        config.packages = std.data-merge.append [
          "github:input-output-hk/cicero/${cfg.sha}#devShell.x86_64-linux"
        ];
      }

      (std.wrapScript "bash" (next: ''
        set -ex
        lint
        ${lib.escapeShellArgs next}
      ''))

      std.nix.build
    ];
}
