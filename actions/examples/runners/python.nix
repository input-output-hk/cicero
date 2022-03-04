{
  std,
  actionLib,
  ...
} @ args:
std.behavior.onUpdate args {
  job = _:
    actionLib.simpleJob args (std.script "python" ''
      print("Hello Python")
    '');
}
