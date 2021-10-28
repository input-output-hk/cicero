{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.10.26.001";
  vendorSha256 = "sha256-2sABy972hEAT3AXucMLTPkL+BGvIA762rlWWzjrQuz8=";

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
