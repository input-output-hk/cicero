{
  flake,
  buildGoModule,
  go-mockery,
}: let
  final = package "sha256-K0Qbz3WDutHDkGYz/0ZpL4Ix8IwfRlu5cT5lXCwr2rE=";
  package = vendorSha256:
    buildGoModule rec {
      pname = "cicero";
      version = "2022.04.19.001";
      inherit vendorSha256;

      passthru.invalidHash =
        package "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

      src = flake.inputs.inclusive.lib.inclusive ../../. [
        ./.
        ../../go.mod
        ../../go.sum
        ../../main.go
        ../../src
      ];

      nativeBuildInputs = [go-mockery];

      preBuild = ''
        go generate ./...
      '';

      ldflags = [
        "-s"
        "-w"
        "-X main.buildVersion=${version}"
        "-X main.buildCommit=${flake.rev or "dirty"}"
      ];
    };
in
  final
