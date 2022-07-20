{ buildGo118Module, fetchFromGitHub }:

buildGo118Module rec {
  name = "go-mockery";
  version = "2.14.0";

  src = fetchFromGitHub {
    owner = "vektra";
    repo = "mockery";
    rev = "v${version}";
    sha256 = "sha256-bOj8NiPYV+XWK2oqC3Kh4NUDCfkXj4hEVO7gN97H+9A=";
  };

  vendorSha256 = "sha256-+40n7OoP8TLyjj4ehBHOD6/SqzJMCHsISE0FrXUL3Q8=";

  # has failing tests even on release tag
  doCheck = false;
}
