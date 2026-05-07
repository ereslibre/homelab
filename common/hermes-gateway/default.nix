{
  config,
  nix-ai-tools,
  ...
}: {
  sops = {
    secrets."hermes/firecrawl" = {};
    templates."hermes-env" = {
      owner = "ereslibre";
      content = ''
        FIRECRAWL_API_KEY=${config.sops.placeholder."hermes/firecrawl"}
      '';
    };
  };

  environment.etc."hermes-gateway.env".source = config.sops.templates."hermes-env".path;

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

    config = {pkgs, ...}: {
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
}
