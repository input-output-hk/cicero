{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.12.03.001";
  vendorSha256 = "sha256-g0LLmw8kajMvmAFuTcgjQKl89ARsXb01aUP0C4gsbhY=";

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
