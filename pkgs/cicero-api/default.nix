{
  haskell-nix,
  supportedSystems,
  src,
  writeShellScript,
  nix,
  jq,
  coreutils,
  git,
  lib,
  system,
}: rec {
  project = haskell-nix.cabalProject' {
    inherit src;
    compiler-nix-name = "ghc924";
    shell.tools.cabal = {};
    materialized = let
      materialized = ./. + "/materialized-${system}";
    in
      if builtins.pathExists materialized
      then materialized
      else null;
  };

  inherit (project.hsPkgs.cicero-api.components.exes) cicero-cli;

  apps.updateAllMaterialized = {
    type = "app";
    program =
      (writeShellScript "updateAllMaterialized" ''
        set -eEuo pipefail
        export PATH="${lib.makeBinPath [nix jq coreutils git]}"
        export NIX_CONFIG="
          allow-import-from-derivation = true
          experimental-features = flakes nix-command
        "
        ${builtins.concatStringsSep "\n" (map (system: ''
            script="$(nix build .#packages.${system}.cicero-api.project.plan-nix.passthru.generateMaterialized --json | jq -r '.[0].outputs.out')"
            echo "Running $script on ./pkgs/cicero-api/materialized-${system}" >&2
            "$script" "./pkgs/cicero-api/materialized-${system}"
          '')
          supportedSystems)}
      '')
      .outPath;
  };

  apps.cicero-cli = {
    type = "app";
    program = "${cicero-cli}/bin/cicero-cli";
  };
}
