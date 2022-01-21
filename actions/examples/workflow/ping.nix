{
  workflow = "ping-pong";

  __functor = { workflow, ... }:
    { std, lib, actionLib, ... }@args: {
      inputs.state = ''
        // set to whatever else to trigger
        "${workflow}": !="ping" & !="pong" & !="ping-pong"
      '';

      output = _: {
        success.${workflow} = "ping";
      };

      job = { state }:
        actionLib.simpleJob args (std.script "bash" ''
          echo 'Triggered by: '${lib.escapeShellArg state.value.${workflow}}
          echo 'Ping!'
        '');
    };
}
