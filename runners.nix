final: prev:

let
  drvName = "workflow-step-script";

  runner = drv:
    prev.writers.writeBashBin drvName ''
      export PATH=${prev.lib.makeBinPath (
        with final; [
          coreutils
          gnutar
          xz
          liftbridge-cli
        ]
      )}
      export CICERO_SCRIPT="$1"
      exec ${drv}
    '';
in

{
  run-bash = runner (
    prev.writers.writeBash drvName ''
      eval "$CICERO_SCRIPT"
    ''
  );

  run-python = runner (
    prev.writers.writePython3 drvName {} ''
      import os
      eval(os.environ['CICERO_SCRIPT'])
    ''
  );

  run-perl = runner (
    prev.writers.writePerl drvName {} ''
      eval $ENV{'CICERO_SCRIPT'};
    ''
  );

  run-js = runner (
    prev.writers.writeJS drvName {} ''
      const process = require('process')
      eval(process.env.CICERO_SCRIPT)
    ''
  );
}
