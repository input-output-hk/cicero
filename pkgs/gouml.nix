{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {

  pname = "gouml";
  version = "0.2.3";
  src = fetchFromGitHub {
    owner = "kazukousen";
    repo = "gouml";
    rev = "${version}";
    hash = "sha256-/ifNt4mdbxbo0Pww6RSMgIRh0NiHO7fe/AVX7p2h4hc=";
  };

  vendorSha256 = "sha256-/NUZoIfbdyI+mJyKtQtzg6ojC8uJklrEq5HXRPdaPwM=";

  doCheck = false;

  meta = with lib; {
    description = "plantUml generator for go";
    homepage = "https://github.com/kazukousen/gouml";
    license = licenses.asl20;
    maintainers = with maintainers; [ rschardt ];
  };
}
