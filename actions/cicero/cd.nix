{
  name,
  std,
  lib,
  actionLib,
  nixpkgsRev,
  ...
} @ args: {
  io = ''
    inputs: ci: match: "cicero/ci": {
      ok:       true
      revision: string

      ref:            "refs/heads/\(default_branch)"
      default_branch: string
    }

    output: success: "${name}": {
      ok:       true
      revision: inputs.ci.value."cicero/ci".revision
    }
  '';

  job = {ci}: let
    inherit (ci.value."cicero/ci") revision;
  in
    std.chain args [
      actionLib.simpleJob

      (std.git.clone {
        sha = revision;
        clone_url = "https://github.com/input-output-hk/cicero";
      })

      {
        config.packages = std.data-merge.append (
          map (pkg: "github:NixOS/nixpkgs/${nixpkgsRev}#${pkg}") [
            "cue"
            "nomad"
          ]
        );
      }

      (std.script "bash" ''
        cue export ./jobs -e jobs.cicero \
          -t env=prod \
          -t sha=${lib.escapeShellArg revision} \
        | nomad job run -
      '')
    ];
}
