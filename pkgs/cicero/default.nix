{
  flake,
  buildGo118Module,
  go-mockery,
}: let
  final = package "sha256-nO1yAWrShkdqmRdaJUso7dQTYBD+GU/1uLtwemgZEz8=";
  package = vendorSha256:
    buildGo118Module rec {
      pname = "cicero";
      version = "2022.04.27.001";
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
