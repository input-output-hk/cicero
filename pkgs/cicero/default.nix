{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.11.09.001";
  vendorSha256 = "sha256-m1ASS7TgoEPg+7StFn9LLt21oHXVE7rxw4nhd2EWROo=";

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
