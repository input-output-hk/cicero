{
  name,
  std,
  lib,
  actionLib,
  nixpkgsRev,
  ...
} @ args: let
  pkg = pkg: "github:NixOS/nixpkgs/${nixpkgsRev}#${pkg}";
in {
  inputs.start = ''
    "${name}": {
      sha: string
      clone_url?: string
      environment?: string
    }
  '';

  job = {start}: let
    cfg = start.value.${name};
  in
    std.chain args [
      actionLib.simpleJob

      (std.git.clone {
        inherit (cfg) sha;
        clone_url = cfg.clone_url or "https://github.com/input-output-hk/cicero";
      })

      {
        resources.memory = 1024;
        config.packages = std.data-merge.append (map pkg ["cue" "nomad"]);
      }

      (std.script "bash" ''
        cue export ./jobs -e jobs.webhooks \
          ${
          lib.optionalString (cfg ? environment)
          "-t env=${lib.escapeShellArg cfg.environment}"
        } \
          -t sha=${lib.escapeShellArg cfg.sha} \
          > job.json

        nomad run job.json
      '')
    ];
}
