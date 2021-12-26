{ name, std, ... } @ args:

{
  # TODO do not pass `id` in top-level `args` so it cannot be interpolated into `inputs`?
  # Or maybe don't pass `id` to the evaluation at all?! Is it needed?
  # If we want to keep passing `id`, add it as arg to `EvaluationService.EvaluateAction(id)`

  inputs = {
    start = ''
      "${name}/start": number
    '';

    "has not run yet" = {
      not = true;
      match = ''
        "${name}": _inputs.start.value."${name}/start"
      '';
    };
  };

  outputs = { start }: {
    success = [ { ${name} = start.value."${name}/start"; } ];
  };

  job = let
    wfLib = import ../../workflows-lib.nix args;
  in inputs: std.chain args [
    wfLib.jobDefaults

    # systemd-nspawn does not like underscore
    (std.escapeNames [ "_" ] [ "-" ])

    std.singleTask

    (std.script "bash" ''
      echo 'Hello Bash'
    '')
  ];
}
