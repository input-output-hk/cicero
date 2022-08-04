{
  workflow = "ping-pong";

  __functor = {workflow, ...}: {
    std,
    lib,
    actionLib,
    ...
  } @ args: {
    io = ''
      // set to whatever else to trigger
      inputs: state: match: "${workflow}": !="ping" & !="pong" & !="ping-pong"

      output: success: "${workflow}": "ping"
    '';

    job = {state}:
      actionLib.simpleJob args (std.script "bash" ''
        echo 'Triggered by: '${lib.escapeShellArg state.value.${workflow}}
        echo 'Ping!'
      '');
  };
}
