{
  config,
  pkgs,
  llm-agents,
  ...
}: {
  sops = {
    secrets."hermes/telegram_allowed_users" = {
      sopsFile = ./secrets.yaml;
    };
    secrets."hermes/telegram_allowed_groups" = {
      sopsFile = ./secrets.yaml;
    };
    secrets."hermes/gogcli_keyring_password" = {
      sopsFile = ./secrets.yaml;
    };
    templates."hermes-env" = {
      owner = "ereslibre";
      content = ''
        TELEGRAM_ALLOWED_USERS=${config.sops.placeholder."hermes/telegram_allowed_users"}
        TELEGRAM_GROUP_ALLOWED_CHATS=${config.sops.placeholder."hermes/telegram_allowed_groups"}
        SEARXNG_URL=http://127.0.0.1:8080
        GOG_KEYRING_PASSWORD=${config.sops.placeholder."hermes/gogcli_keyring_password"}
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
      services.searx = {
        enable = true;
        settings = {
          server = {
            port = 8080;
            bind_address = "127.0.0.1";
            secret_key = "hermes-local-searxng";
            limiter = false;
          };
          search.formats = ["html" "json"];
          engines = [
            {
              name = "brave";
              disabled = true;
            }
            {
              name = "startpage";
              disabled = true;
            }
          ];
        };
      };

      environment.systemPackages =
        (with pkgs; [
          bash
          cacert
          chromium
          coreutils
          findutils
          gnugrep
          gnused
          gogcli
          google-cloud-sdk
          procps
          python3
          # Temporary workaround: hermes calls agent-browser even in CDP mode.
          # https://github.com/NousResearch/hermes-agent/issues/15952
          agent-browser
        ])
        ++ [
          llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent
        ];

      programs.zsh.enable = true;

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
            ExecStartPre = "${pkgs.coreutils}/bin/rm -f /home/ereslibre/.hermes/chrome-debug/SingletonLock /home/ereslibre/.hermes/chrome-debug/SingletonCookie /home/ereslibre/.hermes/chrome-debug/SingletonSocket";
            ExecStart = "${pkgs.chromium}/bin/chromium --remote-debugging-port=9222 --user-data-dir=/home/ereslibre/.hermes/chrome-debug --no-first-run --no-default-browser-check --headless --disable-gpu --disable-dev-shm-usage --no-sandbox";
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
            ExecStart = "${llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent}/bin/hermes gateway run --replace";
            Restart = "on-failure";
            User = "ereslibre";
            WorkingDirectory = "/home/ereslibre";
            EnvironmentFile = "/etc/hermes-gateway.env";
            Environment = [
              "PATH=/run/current-system/sw/bin:/usr/local/bin:/usr/bin:/bin"
              "GOG_KEYRING_BACKEND=file"
            ];
          };
        };
      };

      programs.nix-ld.enable = true;

      system.stateVersion = "25.05";
    };
  };
}
