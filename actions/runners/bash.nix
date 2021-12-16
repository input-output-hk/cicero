{ name, std, ... } @ args:

{
  inputs.last_none.has_run = ''
    "${name}": bool
  '';

  job = inputs: std.chain args [
    std.singleTask
    (std.script "bash" ''
      echo 'Hello Bash'
    '')
  ];
}
