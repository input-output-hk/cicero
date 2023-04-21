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

      vendorHash = "sha256-w8nlWvjfbVfe9MC0SIkCWWjsrr9AnMyV1WTiv5Rludg=";

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
