{ self, pkgs, config, lib, ... }:

{
  imports = [
    self.nixosModule
    self.inputs.driver.nixosModules.nix-driver-nomad
    "${self.inputs.nixpkgs}/nixos/modules/misc/version.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/base.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/headless.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/qemu-guest.nix"
  ];

  networking.hostName = "cicero";

  nixpkgs.overlays = [ self.overlay ];

  nix = {
    systemFeatures = [ "recursive-nix" "nixos-test" ];
    extraOptions = ''
      experimental-features = nix-command flakes ca-references recursive-nix
    '';
  };

  users.users = {
    nixos = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      initialHashedPassword = "";
    };

    root.initialHashedPassword = "";
  };

  security.sudo = {
    enable = lib.mkDefault true;
    wheelNeedsPassword = lib.mkForce false;
  };

  services = {
    cicero.enable = true;

    getty.autologinUser = "nixos";

    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
  };
}
