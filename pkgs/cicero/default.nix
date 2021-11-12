{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.12.001";
  vendorSha256 = "sha256-ZF5zu5S/KPBrRM457AgZRzaqLgcn7e/JlbTxWUZZ4Fk=";

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
