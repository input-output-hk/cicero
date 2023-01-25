{
  perSystem = {
    lib,
    pkgs,
    inputs',
    ...
  }: {
    packages.cicero-evaluator-nix = pkgs.writeShellApplication {
      name = "cicero-evaluator-nix";
      runtimeInputs = with pkgs; [
        coreutils
        jq
        inputs'.nix2container.packages.skopeo-nix2container
      ];
      text = lib.fileContents ./cicero-evaluator-nix.sh;
    };
  };
}
