{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.13.001";
  vendorSha256 = "sha256-pp9ciHTcrcSSV0RGInhuU8180AcnM/jToPVQDj0M/e8=";

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
