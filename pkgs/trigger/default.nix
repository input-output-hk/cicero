{
  flake,
  buildGo118Module,
  glibc,
}: let
  final = package "sha256-nEmRypgTG6WNY3t1BljPiHuwK1UDlhKRFRiP2ldacjU=";
  package = vendorSha256:
    buildGo118Module rec {
      pname = "trigger";
      version = "2022.05.19.001";
      inherit vendorSha256;

      passthru.invalidHash =
        package "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

      src = flake.inputs.inclusive.lib.inclusive ../../. [
        ../../go.mod
        ../../go.sum
        ../../trigger
      ];

      CGO_ENABLED = 0;

      ldflags = [
        "-s"
        "-w"
        "-X main.buildVersion=${version}"
        "-X main.buildCommit=${flake.rev or "dirty"}"
      ];
    };
in
  final
