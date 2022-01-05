final: prev:

let
  commonName = "workflow-action-script";

  runner = drv:
    prev.writers.writeBashBin "${drv.name}-runner" ''
      set -euo pipefail
      export CICERO_SCRIPT=$(< "$1")
      exec ${drv}
    '';
in

{
  run-bash = runner (prev.writers.writeBash "${commonName}-bash" ''
    set -euo pipefail

    if [[ -n "''${CICERO_DEBUG:-}" ]]; then
      set -x
    fi

    # not necessary, just convenience
    export PATH="$PATH:${prev.lib.makeBinPath (with final; [ coreutils ])}"

    eval "$CICERO_SCRIPT"
  '');

  run-python = runner (prev.writers.writePython3 "${commonName}-python" { } ''
    import os
    eval(os.environ['CICERO_SCRIPT'])
  '');

  run-perl = runner (prev.writers.writePerl "${commonName}-perl" { } ''
    eval $ENV{'CICERO_SCRIPT'};
  '');

  run-js = runner (prev.writers.writeJS "${commonName}-js" { } ''
    const process = require('process')
    eval(process.env.CICERO_SCRIPT)
  '');
}
