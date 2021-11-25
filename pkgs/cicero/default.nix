{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.19.003";
  vendorSha256 = "sha256-M1XAsps+u8ktxDALOKVEoP7LDXwbIYs7WZf6SiQxrhM=";

  src = flake.inputs.inclusive.lib.inclusive ../../. [
    ./.
    ../../go.mod
    ../../go.sum
    ../../main.go
    ../../src
  ];

  ldflags = [
    "-s"
    "-w"
    "-X main.buildVersion=${version}"
    "-X main.buildCommit=${flake.rev or "dirty"}"
  ];
}
