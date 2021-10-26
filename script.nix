{ language, script }:

let
  inherit (builtins) getFlake;

  flake = getFlake "github:input-output-hk/cicero";
  pkgs =
    flake.inputs.nixpkgs.legacyPackages.${builtins.currentSystem}
    // flake.legacyPackages.${builtins.currentSystem};

  inherit (flake.inputs.nixpkgs) lib;

  drvName = "workflow-step-script";

  wrap = env: drv:
    pkgs.writers.writeBash drvName (
      let
        vars = lib.concatStringsSep "\n" (
          lib.mapAttrsToList
            (k: v: "export ${k}=${lib.escapeShellArg v}")
            (removeAttrs env [ "PATH" ])
        );
        path = lib.makeBinPath (
          (with pkgs; [ coreutils liftbridge-cli gnutar xz ])
          ++ env.PATH or []
        );
      in
        ''
          ${vars}
          export PATH=${path}
          exec ${drv}
        ''
    );
in

  with pkgs.writers;

  {
    bash = wrap {
      PATH = with pkgs; [ git ];
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    } (writeBash drvName script);

    python = wrap {} (writePython3 drvName {} script);

    perl = wrap {} (writePerl drvName {} script);

    js = wrap {} (writeJS drvName {} script);

    haskell = wrap {} (writeHaskell drvName {} script);

    rust = wrap {} (writeRust drvName {} script);
  }.${language}
