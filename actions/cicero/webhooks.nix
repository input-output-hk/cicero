{ name, std, lib, actionLib, nixpkgsRev, ... } @ args:

let
  pkg = pkg: "github:NixOS/nixpkgs/${nixpkgsRev}#${pkg}";
in

std.behavior.onInputChange "start" name args

{
  inputs.start = ''
    "${name}": start: {
      sha: string
      clone_url?: string
      environment?: string
    }
  '';

  job = inputs: let
    inherit (inputs.start.value.${name}) start;
  in std.chain args [
    actionLib.simpleJob

    (std.git.clone {
      inherit (start) sha;
      clone_url = start.clone_url or "https://github.com/input-output-hk/cicero";
    })

    {
      resources.memory = 1024;
      config.packages = std.data-merge.append (map pkg [ "cue" "nomad" ]);
    }

    (std.script "bash" ''
      cue export ./jobs -e jobs.webhooks \
        ${
          lib.optionalString (start ? environment)
          "-t env=${lib.escapeShellArg start.environment}"
        } \
        -t sha=${lib.escapeShellArg start.sha} \
        > job.json

      nomad run job.json
    '')
  ];
}
