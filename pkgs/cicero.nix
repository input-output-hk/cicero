{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.10.28.001";
  vendorSha256 = "sha256-EA8NPAF6UwUXILOUYb7NaNyBn4zjzkc0lJ69+qKy+j4=";

  src = flake.inputs.inclusive.lib.inclusive ./.. [
    ../src
    ../db
    ../go.mod
    ../go.sum
    ../main.go
  ];

  ldflags = [
    "-s"
    "-w"
    "-X main.buildVersion=${version}"
    "-X main.buildCommit=${flake.rev or "dirty"}"
  ];
}
