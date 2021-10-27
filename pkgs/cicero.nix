{ flake, buildGoModule }:

buildGoModule rec {
  pname = "cicero";
  version = "2021.10.26.001";
  vendorSha256 = "sha256-l/3rsRwop6DcBwZX5eE87ReuDl86Yp5A/j8QbDU1jgk=";

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
