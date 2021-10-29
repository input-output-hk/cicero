{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.10.29.002";
  vendorSha256 = "sha256-q8BOi0Hiv/YPVITGQtxFRnaV3jxKH4X/hkmkmpYFIX4=";

  src = flake.inputs.inclusive.lib.inclusive ../../. [
    ./.
    ../../db
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
