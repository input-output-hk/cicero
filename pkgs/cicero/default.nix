{
  flake,
  system,
  pkgs,
  buildGo118Module,
  go-mockery,
  runCommandNoCC,
}: let
  simple = package "sha256-IxwyLQ1tDVPztA8GQlCmIMZhFWRfDB1TEu+4UTu28mc=";

  package = vendorSha256:
    buildGo118Module rec {
      pname = "cicero";
      version = "2023.01.19.001";
      inherit vendorSha256;

      passthru.invalidHash =
        package "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

      src = flake.inputs.inclusive.lib.inclusive ../../. [
        ./.
        ../../go.mod
        ../../go.sum
        ../../main.go
        ../../src
      ];

      nativeBuildInputs = [go-mockery];

      preBuild = ''
        go generate ./...
      '';

      ldflags = [
        "-s"
        "-w"
        "-X main.buildVersion=${version}"
        "-X main.buildCommit=${flake.rev or "dirty"}"
      ];
    };

  # We cannot use pkgs.nixosTest because
  # it gives us no way to inject specialArgs
  # and _module.args lead to infinite recursion with self.
  schemathesisTest =
    (import "${flake.inputs.nixpkgs}/nixos/lib/testing-python.nix" {
      inherit system pkgs;
      specialArgs.self = flake;
      extraConfigurations = [
        ({lib, ...}: {
          nixpkgs = {
            inherit pkgs;
            overlays = lib.mkAfter [
              (_: _: {cicero = simple;})
            ];
          };
        })
      ];
    })
    .makeTest {
      name = "schemathesis";
      nodes.main = {pkgs, ...}: {
        imports = [../../nixos/configs/cicero.nix];

        environment = {
          # Do not try to connect to Nomad as we have none running.
          etc."cicero/start.args".text = "web";

          systemPackages = [pkgs.schemathesis];
        };

        # The derivation implementing this accesses /var/log/{b,w}tmp at build time
        # (should probably be fixed upstream).
        # Without sandboxing, as is the case in an unprivileged container,
        # this will fail due to file permissions not being open for the nix build user.
        services.logrotate.checkConfig = false;

        systemd.services.postgresql.serviceConfig.TimeoutStartSec = 300;
      };
      testScript = ''
        main.wait_for_unit("cicero")
        main.succeed("schemathesis run http://127.0.0.1:8080/documentation/cicero.json --validate-schema=false --hypothesis-suppress-health-check=too_slow")
      '';
    };
in
  runCommandNoCC simple.name {
    requiredSystemFeatures = ["kvm"];
    inherit (simple) passthru;
  } ''
    ln -s ${simple} $out
    ${schemathesisTest.driver}/bin/nixos-test-driver
  ''
