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

      vendorHash = "sha256-Xv4IG2OUCTk03p6Gy3T7jtdrD2I0OoO0MkRI7Eir47c=";

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
