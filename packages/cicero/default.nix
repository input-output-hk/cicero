{
  self,
  config,
  inputs,
  ...
}: {
  perSystem = perSystem @ {
    inputs',
    lib,
    pkgs,
    system,
    ...
  }: {
    packages = rec {
      default = cicero;

      cicero = let
        package = pkgs.buildGo120Module rec {
          pname = "cicero";
          version = "2023.04.05";
          vendorHash = "sha256-9IhfbWHevgL3xQ2pIUaK0Sx+TS9o+3mZkydeQTGLkeQ=";

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

          passthru.tests = let
            ifSystem = pred: lib.optionalAttrs (pred (lib.systems.parse.mkSystemFromString system));
          in
            with lib.systems.inspect.predicates;
              ifSystem isLinux {
                api = apiTest;
              };
        };

        nixosTestNode = {
          imports = [config.flake.nixosModules.default];

          nixpkgs.overlays = lib.mkForce [
            (_: _: {
              cicero = package;
              inherit (perSystem.config.packages) cicero-evaluator-nix;
            })
          ];

          nix.settings.experimental-features = ["nix-command" "flakes"];

          services = {
            cicero = {
              enable = true;
              args = lib.mkDefault "web";
            };

            # The derivation implementing this accesses /var/log/{b,w}tmp at build time
            # (should probably be fixed upstream).
            # Without sandboxing, as is the case in an unprivileged container,
            # this will fail due to file permissions not being open for the nix build user.
            logrotate.checkConfig = false;
          };

          systemd.services.postgresql.serviceConfig.TimeoutStartSec = 300;
        };

        apiTest = let
          src = inputs.inclusive.lib.inclusive ../.. [
            ../../flake.nix
            ../../flake.lock
            ../../devShells.nix
            ../../lib.nix
            ../../nixos/configs
            ../../nixos/modules
            ../../overlay.nix
            ../../packages
            ../../treefmt.nix
            ../../tullia.nix
          ];
          flakeArchive = inputs:
            lib.flatten (
              map
              (
                input:
                  [input]
                  ++ (flakeArchive (input.inputs or {}))
              )
              (__attrValues inputs)
            );
        in
          pkgs.nixosTest {
            name = "api";
            nodes.main = {
              config,
              pkgs,
              ...
            }: let
              dexPort = 5556;
            in {
              imports = [nixosTestNode];

              system.extraDependencies = [src] ++ flakeArchive inputs;

              environment.systemPackages = [inputs'.nixpkgs-unstable.legacyPackages.hurl];

              services = {
                cicero.args = toString [
                  "web"
                  "--web-cookie-auth"
                  (__toFile "cookie-auth" "aaaaaaaaaaaaaaaa")
                  "--web-cookie-enc"
                  (__toFile "cookie-enc" "aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb")
                  "--web-oidc-providers"
                  (__toFile "oidc-providers" (__toJSON {
                    dex = {
                      issuer = "http://localhost:${toString dexPort}";
                      client-id = "cicero";
                      client-secret = "foo";
                      callback-url = "http://localhost:8080/login/oidc/dex/callback";
                    };
                  }))
                ];

                dex = {
                  enable = true;
                  settings = {
                    issuer = "http://localhost:${toString dexPort}";

                    web.http = "0.0.0.0:${toString dexPort}";

                    storage = {
                      type = "postgres";
                      config = {
                        host = "/var/run/postgresql";
                        inherit (config.services.postgresql) port;
                      };
                    };

                    oauth2.skipApprovalScreen = true;

                    connectors = [
                      {
                        id = "mock-callback";
                        name = "no credentials";
                        type = "mockCallback";
                      }
                    ];

                    staticClients = [
                      {
                        id = "cicero";
                        name = "Cicero";
                        redirectURIs = ["http://localhost:8080/login/oidc/dex/callback"];
                        secret = "foo";
                      }
                    ];
                  };
                };

                postgresql = {
                  ensureDatabases = ["dex"];
                  ensureUsers = [
                    {
                      name = "dex";
                      ensurePermissions."DATABASE dex" = "ALL PRIVILEGES";
                    }
                  ];
                };
              };

              systemd.services = {
                cicero.after = ["dex.service"];

                # Do not start `After=` units (cicero in our case) until dex listens.
                # Neither `ss` nor `netstat` work in the testing VM.
                # while [[ -z "$(${pkgs.iproute}/bin/ss -Htln sport = :${toString dexPort})" ]]; do sleep 1; done
                # until ${pkgs.nettools}/bin/netstat -tln | grep -q ':${toString dexPort}\b'; do sleep 1; done
                dex.serviceConfig.ExecStartPost = pkgs.writeShellScript "dex-listens" ''
                  until ${lib.getExe pkgs.curl} --silent http://localhost:${toString dexPort} > /dev/null; do sleep 1; done
                '';
              };
            };
            testScript = let
              testData = rec {
                timestamp = "2023-03-28T00:00:00Z";
                timestampSql = "2023-03-28 00:00:00";
                actionName = "foo/bar";
                runStatus = "succeeded";
                actionId = "00000000-0000-0000-0000-000000000001";
                invocationId = "00000000-0000-0000-0000-000000000002";
                runId = "00000000-0000-0000-0000-000000000003";
                io = ''
                  inputs: {
                    foo: match: foo: "foo"
                    bar: match: bar: inputs.foo.match.foo
                  }
                  output: {
                    success: "yup"
                    failure: "nope"
                  }
                '';
                fooFact = {
                  id = "00000000-0000-0000-0000-000000000004";
                  value.foo = "foo";
                };
                barFact = {
                  id = "00000000-0000-0000-0000-000000000005";
                  value.bar = fooFact.value.foo;
                };
                bazFact = {
                  id = "00000000-0000-0000-0000-000000000006";
                  value.baz = "baz";
                };
              };
              urlEscape = lib.replaceStrings ["/" " "] ["%2F" "%20"];
              hurlScript1 = __toFile "1.hurl" ''
                GET http://localhost:8080/api/action
                HTTP 200
                Content-Type: application/json; charset=utf-8
                ```json
                []
                ```

                GET http://localhost:8080/api/invocation
                HTTP 200
                Content-Type: application/json; charset=utf-8
                ```json
                []
                ```

                GET http://localhost:8080/api/run
                HTTP 200
                Content-Type: application/json; charset=utf-8
                ```json
                []
                ```

                GET http://localhost:8080/api/fact
                HTTP 405
                ``

                GET http://localhost:8080/api/action/current
                HTTP 200
                Content-Type: application/json; charset=utf-8
                ```json
                []
                ```

                GET http://localhost:8080/login/oidc/dex
                [Options]
                location: true
                HTTP 200

                GET http://localhost:8080/api/action/definition/${urlEscape src}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0]" == "cicero/ci"

                # FIXME Make evaluation work.
                # GET http://localhost:8080/api/action/definition/${urlEscape src}/${urlEscape testData.actionName}/00000000-0000-0000-0000-000000000000
                # HTTP 200
                # Content-Type: application/json; charset=utf-8

                POST http://localhost:8080/api/fact
                Content-Type: application/json; charset=utf-8
                ${__toJSON testData.fooFact.value}
                HTTP 200
                [Asserts]
                jsonpath "$.id" != null

                POST http://localhost:8080/api/fact
                Content-Type: application/json; charset=utf-8
                ${__toJSON testData.barFact.value}
                HTTP 200
                [Asserts]
                jsonpath "$.id" != null
              '';
              hurlScript2 = __toFile "2.hurl" ''
                GET http://localhost:8080/api/fact/${testData.fooFact.id}
                Content-Type: application/json; charset=utf-8
                HTTP 200
                [Asserts]
                jsonpath "$.id" == "${testData.fooFact.id}"
                jsonpath "$.value.foo" == "${testData.fooFact.value.foo}"

                GET http://localhost:8080/api/fact/${testData.barFact.id}
                Content-Type: application/json; charset=utf-8
                HTTP 200
                [Asserts]
                jsonpath "$.id" == "${testData.barFact.id}"
                jsonpath "$.value.bar" == "${testData.barFact.value.bar}"

                GET http://localhost:8080/api/action
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.actionId}"
                jsonpath "$[0].name" == "${testData.actionName}"
                jsonpath "$[0].source" == "${src}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].meta" == null
                jsonpath "$[0].io" != null

                GET http://localhost:8080/api/action/${testData.actionId}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.id" == "${testData.actionId}"
                jsonpath "$.name" == "${testData.actionName}"
                jsonpath "$.source" == "${src}"
                jsonpath "$.created_at" == "${testData.timestamp}"
                jsonpath "$.meta" == null
                jsonpath "$.io" != null

                GET http://localhost:8080/api/action/current
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.actionId}"
                jsonpath "$[0].name" == "${testData.actionName}"
                jsonpath "$[0].source" == "${src}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].meta" == null
                jsonpath "$[0].io" != null

                GET http://localhost:8080/api/action/current/${urlEscape testData.actionName}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.id" == "${testData.actionId}"
                jsonpath "$.name" == "${testData.actionName}"
                jsonpath "$.source" == "${src}"
                jsonpath "$.created_at" == "${testData.timestamp}"
                jsonpath "$.meta" == null
                jsonpath "$.io" != null

                GET http://localhost:8080/api/invocation
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.invocationId}"
                jsonpath "$[0].action_id" == "${testData.actionId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/invocation/${testData.invocationId}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.id" == "${testData.invocationId}"
                jsonpath "$.action_id" == "${testData.actionId}"
                jsonpath "$.created_at" == "${testData.timestamp}"
                jsonpath "$.finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/run
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].nomad_job_id" == "${testData.runId}"
                jsonpath "$[0].invocation_id" == "${testData.invocationId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"
                jsonpath "$[0].status" == "${testData.runStatus}"

                GET http://localhost:8080/api/run/${testData.runId}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.nomad_job_id" == "${testData.runId}"
                jsonpath "$.invocation_id" == "${testData.invocationId}"
                jsonpath "$.created_at" == "${testData.timestamp}"
                jsonpath "$.finished_at" == "${testData.timestamp}"
                jsonpath "$.status" == "${testData.runStatus}"

                GET http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.active" == true
                jsonpath "$.private" == false

                PATCH http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                [FormParams]
                active: false
                HTTP 401

                GET http://localhost:8080/login/oidc/dex
                [Options]
                location: true
                HTTP 200

                PATCH http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                [FormParams]
                active: false
                private: true
                HTTP 204

                GET http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.active" == false
                jsonpath "$.private" == true

                PATCH http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                [FormParams]
                active: true
                HTTP 204

                GET http://localhost:8080/logout
                [Options]
                location: true
                HTTP 200

                GET http://localhost:8080/api/action/name/${urlEscape testData.actionName}
                HTTP 404

                GET http://localhost:8080/api/action/current/${urlEscape testData.actionName}
                HTTP 404

                GET http://localhost:8080/api/action/${testData.actionId}
                HTTP 401

                GET http://localhost:8080/login/oidc/dex
                [Options]
                location: true
                HTTP 200

                GET http://localhost:8080/api/action/${testData.actionId}
                HTTP 200

                POST http://localhost:8080/api/action/match
                Content-Type: multipart/form-data; boundary="boundary"
                ```
                --boundary
                Content-Disposition: form-data; name="io"

                ${testData.io}
                --boundary
                Content-Disposition: form-data; name="fact:foo"

                ${__toJSON testData.fooFact.value}
                --boundary
                Content-Disposition: form-data; name="fact:bar"

                ${__toJSON testData.barFact.value}
                --boundary--
                ```
                HTTP 200
                [Asserts]
                jsonpath "$.runnable" == true
                jsonpath "$.inputs.foo.satisfiedByFact" == "foo"
                jsonpath "$.inputs.foo.matchedAgainstFact.foo.errors" == null
                jsonpath "$.inputs.foo.matchedAgainstFact.bar.errors" count >= 1
                jsonpath "$.inputs.foo.matchedAgainstFactWithDeps.foo.errors" == null
                jsonpath "$.inputs.foo.matchedAgainstFactWithDeps.bar.errors" count >= 1
                jsonpath "$.inputs.foo.dependencies" == null
                jsonpath "$.inputs.bar.satisfiedByFact" == "bar"
                jsonpath "$.inputs.bar.matchedAgainstFact.bar.errors" == null
                jsonpath "$.inputs.bar.matchedAgainstFact.foo.errors" count >= 1
                jsonpath "$.inputs.bar.matchedAgainstFactWithDeps.bar.errors" == null
                jsonpath "$.inputs.bar.matchedAgainstFactWithDeps.foo.errors" count >= 1
                jsonpath "$.inputs.bar.dependencies" count == 1
                jsonpath "$.inputs.bar.dependencies[0]" == "foo"
                jsonpath "$.output.success.unified" == "\"yup\""
                jsonpath "$.output.success.errors" == null
                jsonpath "$.output.failure.unified" == "\"nope\""
                jsonpath "$.output.failure.errors" == null

                GET http://localhost:8080/api/invocation
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.invocationId}"
                jsonpath "$[0].action_id" == "${testData.actionId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/invocation
                [QueryStringParams]
                input: ${testData.fooFact.id}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.invocationId}"
                jsonpath "$[0].action_id" == "${testData.actionId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/invocation
                [QueryStringParams]
                input: ${testData.barFact.id}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].id" == "${testData.invocationId}"
                jsonpath "$[0].action_id" == "${testData.actionId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/invocation/${testData.invocationId}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.id" == "${testData.invocationId}"
                jsonpath "$.action_id" == "${testData.actionId}"
                jsonpath "$.created_at" == "${testData.timestamp}"
                jsonpath "$.finished_at" == "${testData.timestamp}"

                GET http://localhost:8080/api/invocation/${testData.invocationId}/inputs
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.foo" != null
                jsonpath "$.bar" != null

                GET http://localhost:8080/api/invocation/${testData.invocationId}/output
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.success" == "yup"
                jsonpath "$.failure" == "nope"

                GET http://localhost:8080/api/run/${testData.runId}/inputs
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.foo" != null
                jsonpath "$.bar" != null

                GET http://localhost:8080/api/run/${testData.runId}/output
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$.success" == "yup"
                jsonpath "$.failure" == "nope"

                POST http://localhost:8080/api/run/${testData.runId}/fact
                Content-Type: application/json; charset=utf-8
                ${__toJSON testData.bazFact.value}
                HTTP 200
                [Asserts]
                jsonpath "$.id" != null

                GET http://localhost:8080/api/run
                [QueryStringParams]
                input: ${testData.barFact.id}
                HTTP 200
                Content-Type: application/json; charset=utf-8
                [Asserts]
                jsonpath "$" count == 1
                jsonpath "$[0].nomad_job_id" == "${testData.runId}"
                jsonpath "$[0].invocation_id" == "${testData.invocationId}"
                jsonpath "$[0].created_at" == "${testData.timestamp}"
                jsonpath "$[0].finished_at" == "${testData.timestamp}"
                jsonpath "$[0].status" == "${testData.runStatus}"
              '';
            in ''
              main.wait_for_unit("cicero")
              main.wait_for_open_port(8080)
              main.succeed(
                "hurl --test --very-verbose --report-html /tmp/hurlScript1 ${hurlScript1}",
                r"""
                  echo "
                    UPDATE fact SET id = :'fooFactId' WHERE value @? '$.foo';
                    UPDATE fact SET id = :'barFactId' WHERE value @? '$.bar';
                    INSERT INTO action (id, name, source, io, created_at) VALUES (:'actionId', :'actionName', :'src', :'io', :'timestamp');
                    INSERT INTO invocation (id, action_id, created_at, finished_at) VALUES (:'invocationId', :'actionId', :'timestamp', :'timestamp');
                    INSERT INTO invocation_inputs (input_name, invocation_id, fact_id) VALUES
                      ('foo', :'invocationId', (SELECT id FROM fact WHERE value @? '$.foo')),
                      ('bar', :'invocationId', (SELECT id FROM fact WHERE value @? '$.bar'));
                    INSERT INTO run (nomad_job_id, invocation_id, created_at, finished_at, status) VALUES (:'runId', :'invocationId', :'timestamp', :'timestamp', :'runStatus'::run_status);
                  " \
                  | sudo -u cicero psql \
                    --variable src=${lib.escapeShellArg src} \
                    --variable io=${lib.escapeShellArg testData.io} \
                    --variable actionName=${lib.escapeShellArg testData.actionName} \
                    --variable actionId=${lib.escapeShellArg testData.actionId} \
                    --variable invocationId=${lib.escapeShellArg testData.invocationId} \
                    --variable runId=${lib.escapeShellArg testData.runId} \
                    --variable fooFactId=${lib.escapeShellArg testData.fooFact.id} \
                    --variable barFactId=${lib.escapeShellArg testData.barFact.id} \
                    --variable runStatus=${lib.escapeShellArg testData.runStatus} \
                    --variable timestamp=${lib.escapeShellArg testData.timestampSql}
                """,
                "hurl --test --very-verbose --report-html /tmp/hurlScript2 ${hurlScript2}",
              )
              main.copy_from_vm("/tmp/hurlScript1")
              main.copy_from_vm("/tmp/hurlScript2")
            '';
          };
      in
        package;
    };
  };
}
