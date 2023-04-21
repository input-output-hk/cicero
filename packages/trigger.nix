{
  self,
  inputs,
  ...
}: {
  perSystem = {pkgs, ...}: {
    packages.webhook-trigger = pkgs.buildGoModule rec {
      pname = "trigger";
      version = "2022.05.19.001";

      src = inputs.inclusive.lib.inclusive ../. [
        ../go.mod
        ../go.sum
        ../trigger
      ];

      vendorHash = "sha256-6UwNhpR6+gXXgB2s8HLW6NcRluicehXTLJTbgIOGd0M=";

      CGO_ENABLED = 0;

      ldflags = [
        "-s"
        "-w"
        "-X main.buildVersion=${version}"
        "-X main.buildCommit=${self.rev or "dirty"}"
      ];
    };
  };
}
