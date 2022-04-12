{
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

      ref?: "refs/heads/\(default_branch)"
      default_branch?: string
    }
  '';

  job = {start}: let
    cfg = start.value."cicero/ci".start;
  in
    std.chain args [
      actionLib.simpleJob

      (std.github.reportStatus cfg.statuses_url or null)

      (std.git.clone cfg)

      {
        resources = {
          cpu = 1000;
          memory = 1024 * 2;
        };
      }

      std.nix.develop

      (std.wrapScript "bash" (next: ''
        set -ex
        lint
        ${lib.escapeShellArgs next}
      ''))

      std.nix.build
    ];
}
