{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args: {
  io = ''
    inputs: start: match: "cicero/ci": start: {
      clone_url:     string
      sha:           string
      statuses_url?: string

      ref?:            "refs/heads/\(default_branch)"
      default_branch?: string
    }

    let cfg = inputs.start.value."cicero/ci".start
    output: success: "${name}": {
      ok:       true
      revision: cfg.sha

      if cfg.ref != _|_ {
        ref:            cfg.ref
        default_branch: cfg.default_branch
      }
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
