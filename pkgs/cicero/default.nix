{
  flake,
  buildGoModule,
  go-mockery,
}: let
  final = package "sha256-PLr6c9ROt3bNgopN7XNQHLUJWHXgnagO13h8u0qEbYc=";
  package = vendorSha256:
    buildGoModule rec {
      pname = "cicero";
      version = "2022.03.25.001";
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
