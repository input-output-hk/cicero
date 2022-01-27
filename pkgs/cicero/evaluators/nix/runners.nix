pkgs:

let
  commonName = "workflow-action-script";

  runner = drv:
    pkgs.writers.writeBashBin "${drv.name}-runner" ''
      set -euo pipefail
      export CICERO_SCRIPT=$(< "$1")
      exec ${drv}
    '';
in

{
  bash = runner (pkgs.writers.writeBash "${commonName}-bash" ''
    set -euo pipefail

    if [[ -n "''${CICERO_DEBUG:-}" ]]; then
      set -x
    fi

    # not necessary, just convenience
    export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.coreutils ]}"

    eval "$CICERO_SCRIPT"
  '');

  python = runner (pkgs.writers.writePython3 "${commonName}-python" { } ''
    import os
    eval(os.environ['CICERO_SCRIPT'])
  '');

  perl = runner (pkgs.writers.writePerl "${commonName}-perl" { } ''
    eval $ENV{'CICERO_SCRIPT'};
  '');

  js = runner (pkgs.writers.writeJS "${commonName}-js" { } ''
    const process = require('process')
    eval(process.env.CICERO_SCRIPT)
  '');
}
