{
  writeShellApplication,
  coreutils,
  lib,
}:
writeShellApplication {
  name = "cicero-evaluator-nix";
  runtimeInputs = [coreutils];
  text = lib.fileContents ./cicero-evaluator-nix.sh;
}
