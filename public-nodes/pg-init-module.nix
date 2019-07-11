{pkgs, config, lib, ...}: with lib; {
  options.services.pgAggregatedInitScript = mkOption {
    type = types.lines;
    default = "";
    description = ''
      Initial SQL script to run after setting up Postgres.
    '';
  };

  config = {
    services.postgresql.initialScript =
      if config.services.pgAggregatedInitScript == ""
        then null
        else pkgs.writeText "init-pg.sql" config.services.pgAggregatedInitScript;
  };
}
