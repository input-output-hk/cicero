{ name, std, actionLib, ... } @ args:

{
  inputs."has not run yet" = {
    not = true;
    match = ''
      "${name}": bool
    '';
  };

  job = _: actionLib.simpleJob args (std.script "bash" ''
    echo 'Hello Bash'
  '');
}
