{
  self,
  inputs,
  ...
}: {
  perSystem = {pkgs, ...}: {
    packages.webhook-trigger = pkgs.buildGoModule rec {
      pname = "trigger";
      version = "2023.04.20.001";

      src = inputs.inclusive.lib.inclusive ../. [
        ../go.mod
        ../go.sum
        ../trigger
      ];

      vendorHash = "sha256-LhDZ/IduaRv97aQZf83k3nZ3Uobg/YdsRRj1GS7Yzb8=";

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
