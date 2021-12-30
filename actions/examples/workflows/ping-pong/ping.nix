{
  # Used as prefix common to all actions of this workflow.
  workflow = "ping-pong";

  __functor = { workflow, ... }:
    { std, actionLib, ... }@args:
    std.behavior.onInputChange "state" args {
      inputs.state = ''
        // set to whatever else to trigger
        "${workflow}": !="ping" & !="pong" & !="ping-pong"
      '';

      outputs = _: {
        ${workflow} = "ping";
      };

      job = _:
        actionLib.simpleJob args (std.script "bash" ''
          echo 'Ping!'
        '');
    };
}
