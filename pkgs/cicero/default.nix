{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.12.02.001";
  vendorSha256 = "sha256-PiGmRvKz1b2mAIPh3oKeN8BjM0VFAEe0FH2PKjOlSdQ=";

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
