{ name, std, ... } @ args:

{
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
