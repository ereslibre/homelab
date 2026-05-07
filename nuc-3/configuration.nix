{
  config,
  pkgs,
  googleworkspace-cli,
  nix-ai-tools,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../common/jupyterhub
    ../common/aliases
    ../common/cloudflared
    ../common/docker
    ../common/home-node
    ../common/kernel
    ../common/nix
    ../common/nix-github
    ../common/node
    ../common/openwebui
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/synapse
    ../common/users
    ../common/vendor/intel
    # FIXME: bring back
    # ../common/vscode-server
  ];

  boot.tmp = {
    useTmpfs = true;
    tmpfsSize = "20%";
  };

  sops = {
    defaultSopsFile = ./secrets.yaml;
    secrets."hermes/firecrawl" = {};
    templates."hermes-env" = {
      owner = "ereslibre";
      content = ''
        FIRECRAWL_API_KEY=${config.sops.placeholder."hermes/firecrawl"}
      '';
    };
  };

  environment.etc."hermes-gateway.env".source = config.sops.templates."hermes-env".path;

  environment.systemPackages =
    (with pkgs; [
      chromium
      esphome
      espup
      google-cloud-sdk
    ])
    ++ [
      googleworkspace-cli.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

  services = {
    caddy = {
      enable = true;
      globalConfig = ''
        auto_https disable_redirects
      '';
      virtualHosts = {
        "openwebui.ereslibre.net".extraConfig = ''
          tls internal
          reverse_proxy http://openwebui:8080
        '';
        "jupyter.ereslibre.net".extraConfig = ''
          tls internal
          reverse_proxy http://localhost:8000
        '';
        "matrix.ereslibre.net".extraConfig = ''
          tls internal
          reverse_proxy http://192.168.100.13:8009
        '';
      };
    };
  };

  networking = {
    hostName = "nuc-3";
    nat = {
      # Enable NAT on containers so that they have external
      # connectivity.
      enable = true;
      internalInterfaces = ["ve-+"];
      externalInterface = "enp2s0";
    };
  };

  containers.hermes-gateway = {
    autoStart = true;
    privateNetwork = false;

    # Expose the same home directory inside the container as on the host so
    # hermes sees the expected user state and paths.
    bindMounts = {
      "/home/ereslibre" = {
        hostPath = "/home/ereslibre";
        isReadOnly = false;
      };
      "/etc/hermes-gateway.env" = {
        hostPath = "/etc/hermes-gateway.env";
        isReadOnly = true;
      };
    };

    config = {
      pkgs,
      ...
    }: {
      programs.chromium.enable = true;

      environment.systemPackages = with pkgs; [
        bash
        chromium
        coreutils
        findutils
        gnugrep
        gnused
        procps
      ];

      users = {
        mutableUsers = false;
        allowNoPasswordLogin = true;
        users.ereslibre = {
          isNormalUser = true;
          uid = 1000;
          home = "/home/ereslibre";
          createHome = false;
        };
      };

      systemd.services = {
        chromium-cdp = {
          description = "Chromium CDP Remote Debugging";
          after = ["network-online.target"];
          wants = ["network-online.target"];
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            Type = "simple";
            ExecStart = "${pkgs.chromium}/bin/chromium --remote-debugging-port=9222 --user-data-dir=/home/ereslibre/.hermes/chrome-debug --no-first-run --no-default-browser-check --headless";
            Restart = "on-failure";
            User = "ereslibre";
          };
        };
        hermes-gateway = {
          description = "Hermes Gateway";
          after = ["network-online.target" "chromium-cdp.service"];
          wants = ["network-online.target" "chromium-cdp.service"];
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            Type = "simple";
            ExecStart = "${nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent}/bin/hermes gateway run --replace";
            Restart = "on-failure";
            User = "ereslibre";
            WorkingDirectory = "/home/ereslibre/.hermes";
            EnvironmentFile = "/etc/hermes-gateway.env";
          };
        };
      };

      system.stateVersion = "25.05";
    };
  };

  systemd.services = {
    iptables-masquerade = {
      # Provide connectivity on the containers; this masquerade rule is
      # not added automatically.
      description = "Add iptables NAT MASQUERADE rule for container NAT";
      wants = ["network-online.target"];
      after = ["network-online.target"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''
          /run/current-system/sw/bin/iptables -t nat -A POSTROUTING -o enp2s0 -j MASQUERADE
        '';
      };
      wantedBy = ["multi-user.target"];
    };
  };

  virtualisation.docker.rootless.enable = pkgs.lib.mkForce false;

  users.users.ereslibre.extraGroups = ["video"]; # surpillance experiments

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
