{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args:
std.behavior.onInputChange "run" name args {
  io = ''
    // arbitrary value that triggers this run
    inputs: run: match: "${name}": _
  '';

  job = {run}:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Running once because my input has been updated by fact '${lib.escapeShellArg run.id}'.'
    '');
}
