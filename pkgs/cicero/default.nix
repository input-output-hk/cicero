{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.01.002";
  vendorSha256 = "sha256-2YzkILh3F5J8Lw4zsPRoaL827HOu0PkV/lF1JjI1s6s=";

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
