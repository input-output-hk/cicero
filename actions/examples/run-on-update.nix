{ name, id, std, lib } @ args:

{
  inputs = {
    old = {
      optional = true;
      match = ''
        "${name}": !="${id}"
      '';
    };

    "do not retry on failure" = {
      not = true;
      match = ''
        "${name}": false
      '';
    };
  };

  outputs = _: {
    success = [ { ${name} = id; } ];
  };

  job = let
    wfLib = import ../../workflows-lib.nix args;
  in { old ? null }: std.chain args [
    wfLib.jobDefaults

    # systemd-nspawn does not like underscore
    (std.escapeNames [ "_" ] [ "-" ])

    std.singleTask

    (std.script "bash" (''
      echo 'Hi, I am the Action named "'${lib.escapeShellArg name}'".'
    '' + (
      if old == null then ''
        echo 'I run because I was created for the first time'.
      '' else ''
        echo 'I run because I was updated (recreated with the same name).'
        echo 'My old version was: '${lib.escapeShellArg old.id}
        echo 'It ran in Nomad job '${lib.escapeShellArg old.run_id}' at '${lib.escapeShellArg old.created_at}'.'
      ''
    ) + ''
      echo 'My new version is: '${lib.escapeShellArg id}
      echo 'See you on my next update!'
    ''))
  ];
}
