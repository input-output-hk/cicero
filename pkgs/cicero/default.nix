{ flake, buildGoModule, go-mockery }:

buildGoModule rec {
  pname = "cicero";
  version = "2022.02.17.001";
  vendorSha256 = "sha256-xSzSUYlBxjzRMP+TPzkWQvuuMbkp4IYD1B8VPRn7xzk=";

  src = flake.inputs.inclusive.lib.inclusive ../../. [
    ./.
    ../../go.mod
    ../../go.sum
    ../../main.go
    ../../src
  ];

  nativeBuildInputs = [ go-mockery ];

  preBuild = ''
    go generate ./...
  '';

  ldflags = [
    "-s"
    "-w"
    "-X main.buildVersion=${version}"
    "-X main.buildCommit=${flake.rev or "dirty"}"
  ];
}
