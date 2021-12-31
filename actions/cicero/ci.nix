{ name, std, lib, actionLib, ... } @ args:

std.behavior.onInputChange "start" name args

{
  inputs.start = ''
    "${name}": start: {
      clone_url: string
      sha: string
      statuses_url?: string
    }
  '';

  job = { start }: let
    cfg = start.value.${name}.start;
  in std.chain args [
    actionLib.simpleJob

    (lib.optionalAttrs (cfg ? statuses_url)
      (std.github.reportStatus cfg.statuses_url))

    (std.git.clone cfg)

    {
      resources = {
        memory = 4 * 1024;
        cpu = 16000;
      };
    }

    std.nix.develop
    (std.wrapScript "bash" (next: ''
      lint
      ${lib.escapeShellArg next}
    ''))

    std.nix.build
  ];
}
