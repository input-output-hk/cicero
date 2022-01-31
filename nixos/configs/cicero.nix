{
  self,
  pkgs,
  ...
}: {
  imports = [
    self.nixosModule
    self.inputs.driver.nixosModules.nix-driver-nomad
  ];

  nixpkgs.overlays = [ self.overlay ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  networking.firewall.enable = false;

  services = {
    cicero.enable = true;

    postgresql.enableTCPIP = true;
  };
}
