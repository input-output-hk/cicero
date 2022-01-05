{ std, actionLib, ... } @ args:

std.behavior.onUpdate args {
  job = _: actionLib.simpleJob args (std.script "js" ''
    console.log('Hello JS')
  '');
}
