{
  lib,
  writeShellApplication,
  coreutils,
  jq,
  skopeo-nix2container,
}:

writeShellApplication {
  name = "cicero-evaluator-nix";
  runtimeInputs = [coreutils jq skopeo-nix2container];
  text = lib.fileContents ./cicero-evaluator-nix.sh;
}
