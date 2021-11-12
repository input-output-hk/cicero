{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "liftbridge";
  version = "1.7.0";

  src = fetchFromGitHub {
    owner = "liftbridge-io";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-S6vh5L1/yHQ8ApNbxiBpsVonl7Tc3MUzBYoPze88JH4=";
  };

  vendorSha256 = "sha256-c+7B9rqf8GdVBYj/4BOtBwnrbu+h44OLH0bgVjP25L0=";

  doCheck = false;

  meta = with lib; {
    description = "Lightweight, fault-tolerant message streams.";
    homepage = "https://liftbridge.io";
    license = licenses.asl20;
    maintainers = with maintainers; [ manveru ];
  };
}
