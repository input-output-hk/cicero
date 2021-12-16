rec {
  workflow = "ping_pong";

  inputs = args: {
    latest.start = ''
      "${workflow}/start": true
    '';
    latest_none."has run" = ''
      "${args.name}": bool
    '';
  };

  job = std: args:
    inputs: std.chain args [
      std.singleTask
      (std.script "bash" ''
        echo 'running ${args.name} #${args.id}'
      '')
    ];

  __functor = _: { std, ... } @ args: {
    inputs = inputs args;
    job = job std args;
  };
}
