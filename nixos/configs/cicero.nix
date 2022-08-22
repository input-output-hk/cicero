{
  self,
  pkgs,
  ...
}: {
  imports = [self.nixosModule];

  nixpkgs.overlays = [self.overlay];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  networking.firewall.enable = false;

  services = {
    cicero.enable = true;

    postgresql.enableTCPIP = true;
  };
}
