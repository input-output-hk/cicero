{inputs, ...}: {
  perSystem = {system, ...}: {
    packages.schemathesis = let
      nixpkgs = __getFlake github:NixOS/nixpkgs/294ef54a1e8cdcdd298c79edbdb3713ceae46988;
      pkgs = nixpkgs.legacyPackages.${system}.extend inputs.poetry2nix.overlay;
    in
      pkgs.poetry2nix.mkPoetryApplication {
        projectDir = pkgs.fetchFromGitHub {
          owner = "schemathesis";
          repo = "schemathesis";
          rev = "v3.12.1";
          sha256 = "sha256-iU1tsA9MKKH/zjuBxD5yExJOPoL2V/OG3WYc9w0do9I=";
        };
        python = pkgs.python39;
      };
  };
}
