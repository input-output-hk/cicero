{ name, id, std, lib, actionLib } @ args:

# TODO provide std functions for patterns like this
# (a function that creates the entire attrset with `inputs` and `outputs`)

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

  job = { old ? null }:
    actionLib.simpleJob args (std.script "bash" (''
      echo 'Hi, I am the Action named "'${lib.escapeShellArg name}'".'
      echo
    '' + (
      if old == null then ''
        echo 'I run because I was created for the first time'.
      '' else ''
        echo 'I run because I was updated (recreated with the same name).'
        echo 'My old ID was: '${lib.escapeShellArg old.value.${name}}
        echo 'It ran in Nomad job '${lib.escapeShellArg old.run_id}' and finished at '${lib.escapeShellArg old.created_at}'.'
      ''
    ) + ''
      echo
      echo 'My new ID is: '${lib.escapeShellArg id}
      echo 'See you on my next update!'
    ''));
}
