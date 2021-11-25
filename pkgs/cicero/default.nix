{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.25.001";
  vendorSha256 = "sha256-NVZW+GobgjxjZSykbasSJoXe3dBtocX/90YtYfRAA2k=";

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
