{pkgs, ...}: let
  dbName = "matrix-synapse";
in {
  services.matrix-synapse = {
    enable = true;
    settings = {
      database.name = "psycopg2";
      listeners = [
        {
          bind_addresses = [
            "::1"
          ];
          port = 8448;
          resources = [
            {
              names = ["client" "federation"];
              compress = true;
            }
          ];
          tls = false;
          type = "http";
          x_forwarded = true;
        }
      ];
    };
  };
  services.postgresql = {
    enable = true;
    initialScript = pkgs.writeText "setup-database" ''
      CREATE ROLE "matrix-synapse" WITH LOGIN;
      CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse" TEMPLATE template0 LC_COLLATE = "C" LC_CTYPE = "C";
    '';
  };
}
