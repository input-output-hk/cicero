{ config, lib, pkgs, ... }:

let
  cfg = config.services.cicero;
in

{
  options.services.cicero = with lib; {
    enable = mkEnableOption "cicero";

    package = mkOption {
      type = types.package;
      default = pkgs.cicero;
    };

    postgres = mkOption {
      type = with types; nullOr str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.cicero = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig.ExecStart = "${cfg.package}/bin/cicero start";
      environment.DATABASE_URL = lib.mkIf (cfg.postgres != null) cfg.postgres;
    };

    postgresql = lib.mkIf (cfg.postgres == null) {
      enable = true;
      initialScript = pkgs.writeText "cicero-init.sql" ''
        CREATE DATABASE cicero;

        CREATE USER cicero;
        GRANT ALL PRIVILEGES ON DATABASE cicero TO cicero;

        CREATE ROLE cicero_api;
      '';
    };
  };
}
