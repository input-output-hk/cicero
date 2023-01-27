{
  flake.nixosModules = rec {
    cicero = import ./cicero.nix;
    default = cicero;
  };
}
