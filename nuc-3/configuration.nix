{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/home-node
    ../common/nixos
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/synapse-server
    ../common/users
    ../common/vendor/intel
    # FIXME: Uncomment when https://github.com/NixOS/nixpkgs/blob/f52dfae760a7135cbb66a38ab60d926b5557ecc3/pkgs/servers/openvscode-server/default.nix#L268-L269 is fixed
    # ../common/vscode-server
  ];

  sops.defaultSopsFile = ./secrets.yaml;
  sops.secrets = {
    "nix-access-tokens" = {
      owner = "ereslibre";
      group = "users";
    };
    "ereslibre-social-cloudflare-tunnel.json" = {};
    "matrix-synapse-registration-shared-secret.yaml" = {
      owner = "matrix-synapse";
      group = "matrix-synapse";
    };
  };

  services.open-webui = {
    enable = true;
    host = "0.0.0.0";
    environment = {
      OLLAMA_API_BASE_URL = "http://hulk.ereslibre.net:11434";
      WEBUI_AUTH = "False";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
    };
  };

  services.matrix-synapse = {
    extraConfigFiles = [
      config.sops.secrets."matrix-synapse-registration-shared-secret.yaml".path
    ];
    settings = {
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
    };
  };

  networking.hostName = "nuc-3";

  nix.extraOptions = ''
    # Access tokens
    !include ${config.sops.secrets.nix-access-tokens.path}
  '';

  users.users.ereslibre.extraGroups = ["video"]; # surpillance experiments

  services.cloudflared = {
    enable = true;
    tunnels = {
      "5b97ef13-505f-41dc-82ff-b16cdce3a46b" = {
        credentialsFile = config.sops.secrets."ereslibre-social-cloudflare-tunnel.json".path;
        default = "http_status:404";
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
