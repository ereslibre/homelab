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
    ../common/vscode-server
  ];

  sops.defaultSopsFile = ./secrets.yaml;
  sops.secrets = {
    "ereslibre-social-cloudflare-tunnel.json" = {
      owner = config.services.cloudflared.user;
      group = config.services.cloudflared.group;
    };
    "matrix-synapse-registration-shared-secret.yaml" = {
      owner = "matrix-synapse";
      group = "matrix-synapse";
    };
  };

  services.matrix-synapse = {
    extraConfigFiles = [
      config.sops.secrets."matrix-synapse-registration-shared-secret.yaml".path
    ];
    settings = {
      server_name = "ereslibre.social";
    };
  };

  networking.hostName = "nuc-3";

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
