{ std, actionLib, ... } @ args:

std.behavior.onUpdate args {
  job = _: actionLib.simpleJob args (std.script "perl" ''
    print STDOUT "Hello Perl\n"
  '');
}
