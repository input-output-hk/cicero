{ name, std, lib, actionLib, ... } @ args:

std.behavior.onInputChange "run" name args {
  inputs.run = ''
    // arbitrary value that identifies this run
    "${name}/run": _
  '';

  job = { run }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Running once because I have not run with "'${lib.escapeShellArg run.value."${name}/run"}'" yet.'
    '');
}
