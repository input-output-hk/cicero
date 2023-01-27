{
  self,
  config,
  inputs,
  getSystem,
  ...
}: {
  perSystem = {
    lib,
    pkgs,
    system,
    ...
  }: {
    packages = rec {
      default = cicero;
      cicero = let
        package = pkgs.buildGo118Module rec {
          pname = "cicero";
          version = "2023.01.19.001";
          vendorHash = "sha256-IxwyLQ1tDVPztA8GQlCmIMZhFWRfDB1TEu+4UTu28mc=";

          src = inputs.inclusive.lib.inclusive ../.. [
            ../../go.mod
            ../../go.sum
            ../../main.go
            ../../src
          ];

          nativeBuildInputs = with pkgs; [go-mockery];

          preBuild = ''
            go generate ./...
          '';

          ldflags = [
            "-s"
            "-w"
            "-X main.buildVersion=${version}"
            "-X main.buildCommit=${self.rev or "dirty"}"
          ];

          passthru.tests.schemathesis = schemathesisTest;
        };

        # We cannot use pkgs.nixosTest because
        # it gives us no way to inject specialArgs
        # and _module.args lead to infinite recursion with self.
        schemathesisTest =
          (import "${inputs.nixpkgs}/nixos/lib/testing-python.nix" {
            inherit system pkgs;
            extraConfigurations = [
              ({lib, ...}: {
                nixpkgs = {
                  inherit pkgs;
                  overlays = lib.mkForce [
                    (_: _: {cicero = package;})
                  ];
                };
              })
            ];
          })
          .makeTest {
            name = "schemathesis";
            nodes.main = {pkgs, ...}: {
              imports = [config.flake.nixosModules.default];

              services = {
                cicero = {
                  enable = true;
                  args = "web";
                };

                # The derivation implementing this accesses /var/log/{b,w}tmp at build time
                # (should probably be fixed upstream).
                # Without sandboxing, as is the case in an unprivileged container,
                # this will fail due to file permissions not being open for the nix build user.
                logrotate.checkConfig = false;
              };

              environment.systemPackages = [(getSystem system).packages.schemathesis];

              systemd.services.postgresql.serviceConfig.TimeoutStartSec = 300;
            };
            testScript = ''
              main.wait_for_unit("cicero")
              main.succeed("schemathesis run http://127.0.0.1:8080/documentation/cicero.json --validate-schema=false --hypothesis-suppress-health-check=too_slow")
            '';
          };
      in
        package;
    };
  };
}
