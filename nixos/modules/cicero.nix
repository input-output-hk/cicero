{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.cicero;
in {
  options.services.cicero = with lib; {
    enable = mkEnableOption "cicero";

    package = mkOption {
      type = types.package;
      default = pkgs.cicero;
    };

    args = mkOption {
      type = types.str;
      default = "";
    };

    postgres = {
      enable =
        mkEnableOption ''
          local postgres database.
          If you disable this make sure the database
          Cicero connects to has the extensions
          <code>pgcrypto</code> and <code>lo</code>.
        ''
        // {default = true;};

      url = mkOption {
        type = types.str;
        default = "postgres://cicero:@cicero/cicero?host=/run/postgresql";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.cicero = {
      wantedBy = ["multi-user.target"];
      after = ["network.target"] ++ lib.optional cfg.postgres.enable "postgresql.service";
      wants = lib.optional cfg.postgres.enable "postgresql.service";

      serviceConfig = {
        User = "cicero";
        ConfigurationDirectory = "cicero";
      };

      path = [cfg.package pkgs.dbmate];

      preStart = ''
        dbmate \
          --no-dump-schema \
          --migrations-dir ${../../db/migrations} \
          up
      '';

      script = ''
        argsFile="$CONFIGURATION_DIRECTORY"/start.args
        if [[ -f "$argsFile" ]]; then
          args=$(< "$argsFile")
        fi

        cicero start $args "$@"
      '';
      scriptArgs = cfg.args;

      environment.DATABASE_URL = cfg.postgres.url;
    };

    users = {
      users.cicero = {
        isSystemUser = true;
        group = "cicero";
      };

      groups.cicero = {};
    };

    services.postgresql = lib.mkIf cfg.postgres.enable {
      enable = true;
      ensureDatabases = ["cicero"];
      ensureUsers = [
        {
          name = "cicero";
          ensurePermissions."DATABASE cicero" = "ALL PRIVILEGES";
        }
        {name = "cicero_api";}
      ];

      identMap = "cicero cicero cicero";
      authentication = "local cicero all ident map=cicero";
    };

    systemd.services.postgresql = lib.mkIf cfg.postgres.enable {
      # starting with postgres 13 these can be created without being superuser
      postStart = lib.mkIf (lib.versionOlder (lib.getVersion config.services.postgresql.package) "13") ''
        $PSQL -d cicero <<'EOF'
        CREATE EXTENSION IF NOT EXISTS pgcrypto;
        CREATE EXTENSION IF NOT EXISTS lo;
        EOF
      '';
    };
  };
}
