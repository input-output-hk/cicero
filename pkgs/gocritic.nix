{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {

  pname = "gocritic";
  version = "8ed57f9b24027a2258017778dc059963bacadd4d";
  src = fetchFromGitHub {
    owner = "go-critic";
    repo = "go-critic";
    rev = "${version}";
    hash = "sha256-AsDPnWHPurHjBsMLb8TVbM2ydYDCOPk1uj8EvT9vQxk=";
  };

  vendorSha256 = "sha256-PSW8oQw2A7OcHEpPj0nmhxX4iC6MiUjipga7FmNFwwo=";

  doCheck = false;

  meta = with lib; {
    description = "The most opinionated Go source code linter for code audit.";
    homepage = "https://github.com/go-critic/go-critic";
    license = licenses.mit;
    maintainers = with maintainers; [ manveru ];
  };
}

