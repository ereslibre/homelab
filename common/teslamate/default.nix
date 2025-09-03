{
  config,
  sops-nix,
  teslamate,
  ...
}: {
  sops.secrets = {
    "teslamate-private-age-key" = {};
  };

  environment.etc."teslamate/private-age-key" = {
    source = config.sops.secrets."teslamate-private-age-key".path;
    mode = "0400";
  };

  containers.teslamate = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";

    bindMounts."/etc/teslamate" = {
      hostPath = "/etc/teslamate";
      isReadOnly = true;
    };

    config = {
      config,
      pkgs,
      ...
    }: {
      imports = [
        sops-nix.nixosModules.sops
        teslamate.nixosModules.default
      ];

      networking.firewall.allowedTCPPorts = [1883 3000 4000];

      sops = {
        age.keyFile = "/etc/teslamate/private-age-key";
        defaultSopsFile = ./secrets.yaml;
        secrets = {
          "db-password" = {};
          "secrets" = {};
        };
        templates."setup-teslamate-database.sql" = {
          content = ''
            CREATE ROLE teslamate WITH SUPERUSER LOGIN PASSWORD '${config.sops.placeholder."db-password"}';
            CREATE DATABASE teslamate WITH OWNER teslamate;
          '';
        };
      };

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_17;
        initialScript = config.sops.templates."setup-teslamate-database.sql".path;
      };

      services.teslamate = {
        enable = true;
        secretsFile = config.sops.secrets."secrets".path;
        autoStart = true;
        listenAddress = "0.0.0.0";
        virtualHost = "teslamate.ereslibre.net";

        postgres = {
          enable_server = false;
          user = "teslamate";
          database = "teslamate";
          host = "127.0.0.1";
          port = 5432;
        };

        grafana = {
          enable = true;
          listenAddress = "0.0.0.0";
          urlPath = "/";
        };

        mqtt = {
          enable = true;
          host = "0.0.0.0";
          port = 1883;
        };
      };

      system.stateVersion = "25.05";
    };
  };
}
