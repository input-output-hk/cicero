{
  perSystem = {
    lib,
    pkgs,
    ...
  }: {
    packages.go-critic = pkgs.buildGoModule rec {
      pname = "go-critic";
      version = "0.6.5";

      src = pkgs.fetchFromGitHub {
        owner = "go-critic";
        repo = "go-critic";
        rev = "v${version}";
        hash = "sha256-9kv2QxNCokOzWq9lK5q0R7LX/3mvBp7PPWVZ66T3FRQ=";
      };

      vendorHash = "sha256-eirYLCLMEDqACk9U4S0bURXzZI1PmROsmHRm/IZGUec=";

      subPackages = ["cmd/gocritic"];

      doCheck = false;

      meta = with lib; {
        description = "The most opinionated Go source code linter for code audit.";
        homepage = "https://github.com/go-critic/go-critic";
        license = licenses.mit;
        maintainers = with maintainers; [manveru];
      };
    };
  };
}
