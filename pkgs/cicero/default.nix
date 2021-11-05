{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.05.001";
  vendorSha256 = "sha256-MRC6JuZ+pCQ900DJgTme9xs/LeuaauHPkECwjoxMDWk=";

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
