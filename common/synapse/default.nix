{
  config,
  pkgs,
  sops-nix,
  ...
}: let
  dbName = "matrix-synapse";
in {
  sops.secrets = {
    "synapse-private-age-key" = {};
  };

  environment.etc."synapse/private-age-key" = {
    source = config.sops.secrets."synapse-private-age-key".path;
    mode = "0400";
  };

  containers.synapse = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.12";
    localAddress = "192.168.100.13";

    bindMounts."/etc/synapse" = {
      hostPath = "/etc/synapse";
      isReadOnly = true;
    };

    config = {
      config,
      pkgs,
      ...
    }: {
      imports = [
        sops-nix.nixosModules.sops
      ];

      networking.firewall.allowedTCPPorts = [8008 8448];

      sops = {
        age.keyFile = "/etc/synapse/private-age-key";
        defaultSopsFile = ./secrets.yaml;
        secrets = {
          "matrix-synapse-registration-shared-secret.yaml" = {
            owner = "matrix-synapse";
            group = "matrix-synapse";
          };
        };
      };

      services.matrix-synapse = {
        enable = true;
        extraConfigFiles = [
          config.sops.secrets."matrix-synapse-registration-shared-secret.yaml".path
        ];
        settings = {
          database.name = "psycopg2";
          public_baseurl = "https://matrix.ereslibre.social";
          server_name = "ereslibre.social";
          allow_public_rooms_over_federation = true;
          enable_metrics = true;
          enable_registration = false;
          url_preview_enabled = true;
          url_preview_ip_range_blacklist = [
            "127.0.0.0/8"
            "10.0.0.0/8"
            "172.16.0.0/12"
            "192.168.0.0/16"
            "100.64.0.0/10"
            "192.0.0.0/24"
            "169.254.0.0/16"
            "192.88.99.0/24"
            "198.18.0.0/15"
            "192.0.2.0/24"
            "198.51.100.0/24"
            "203.0.113.0/24"
            "224.0.0.0/4"
            "::1/128"
            "fe80::/10"
            "fc00::/7"
            "2001:db8::/32"
            "ff00::/8"
            "fec0::/10"
          ];
          listeners = [
            {
              bind_addresses = [
                "0.0.0.0"
              ];
              port = 8008;
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
        initialScript = pkgs.writeText "setup-synapse-database.sql" ''
          CREATE ROLE "matrix-synapse" WITH LOGIN;
          CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse" TEMPLATE template0 LC_COLLATE = "C" LC_CTYPE = "C";
        '';
      };

      system.stateVersion = "25.05";
    };
  };
}
