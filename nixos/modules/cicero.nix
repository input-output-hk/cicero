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

    evaluators = mkOption {
      type = with types; listOf package;
      default = [pkgs.cicero-evaluator-nix];
    };

    transformers = mkOption {
      type = with types; listOf package;
      default = [];
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
        CacheDirectory = "cicero";
      };

      path = [cfg.package pkgs.dbmate config.nix.package] ++ cfg.evaluators ++ cfg.transformers;

      preStart = ''
        dbmate \
          --no-dump-schema \
          --migrations-dir ${../../db/migrations} \
          up
      '';

      script = let
        exeNames = map (e: baseNameOf (lib.getExe e));
      in ''
        argsFile="$CONFIGURATION_DIRECTORY"/start.args
        if [[ -f "$argsFile" ]]; then
          args=$(< "$argsFile")
        fi

        export XDG_CACHE_HOME="$CACHE_DIRECTORY"

        # List `$args` first so positional arguments work as expected!
        cicero start $args "$@" \
          --evaluators ${lib.escapeShellArgs (map (lib.removePrefix "cicero-evaluator-") (exeNames cfg.evaluators))} \
          ${lib.optionalString (cfg.transformers != []) "--transform ${lib.escapeShellArgs (exeNames cfg.transformers)}"}
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

      authentication = "local cicero all peer map=cicero";
      identMap = "cicero cicero cicero";
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
