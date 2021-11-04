{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.01.002";
  vendorSha256 = "sha256-IpYrBwt1/iUQ8oUeCm4qod2HnsPO+8OZ+WLQpjR6nGY=";

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
