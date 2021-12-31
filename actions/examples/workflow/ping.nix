{
  workflow = "ping-pong";

  __functor = { workflow, ... }:
    { std, lib, actionLib, ... }@args:
    std.behavior.onInputChange "state" workflow args {
      inputs.state = ''
        // set to whatever else to trigger
        "${workflow}": !="ping" & !="pong" & !="ping-pong"
      '';

      outputs = _: {
        success.${workflow} = "ping";
      };

      job = { state }:
        actionLib.simpleJob args (std.script "bash" ''
          echo 'Triggered by: '${lib.escapeShellArg state.value.${workflow}}
          echo 'Ping!'
        '');
    };
}
