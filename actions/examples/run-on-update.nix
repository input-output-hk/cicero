{ name, id, std, lib, actionLib } @ args:

std.chain args [
  std.behavior.onUpdate
  (std.behavior.stopOnFailure id)
  (_: _: {
    inputs.old = {
      optional = true;
      match = ''
        "_behavior": once: !="${id}"
      '';
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
          echo 'My old ID was: '${lib.escapeShellArg old.value._behavior.once}
          echo 'It ran in Nomad job '${lib.escapeShellArg old.run_id}' and finished at '${lib.escapeShellArg old.created_at}'.'
        ''
      ) + ''
        echo
        echo 'My new ID is: '${lib.escapeShellArg id}
        echo 'See you on my next update!'
      ''));
  })
]
