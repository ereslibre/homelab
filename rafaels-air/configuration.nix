{
  pkgs,
  lib,
  ...
}: let
  user = "ereslibre";
  userHome = "/Users/${user}";
in {
  imports = [
    ../common/nixos
    ../common/tailscale
  ];

  environment = {
    shells = with pkgs; [zsh];
    userLaunchAgents = {
      "es.ereslibre.emacs.plist" = {
        enable = true;
        text = ''
          <?xml version="1.0" encoding="UTF-8"?>
          <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
          <plist version="1.0">
          <dict>
            <key>EnvironmentVariables</key>
            <dict>
              <key>XDG_RUNTIME_DIR</key>
              <string>${userHome}/.emacs.d</string>
            </dict>
            <key>Label</key>
            <string>es.ereslibre.es.emacs</string>
            <key>ProgramArguments</key>
            <array>
              <string>${pkgs.emacs}/bin/emacs</string>
              <string>--fg-daemon</string>
            </array>
            <key>RunAtLoad</key>
            <true/>
            <key>KeepAlive</key>
            <true/>
            <key>LSUIElement</key>
            <true/>
          </dict>
          </plist>
        '';
      };
    };
  };

  programs.zsh.enable = true;

  home-manager.users.${user}.home = {
    packages = with pkgs; [ollama];
    sessionVariables = {
      OLLAMA_HOST = "hulk.ereslibre.net";
    };
  };

  users.users.${user} = {
    createHome = true;
    home = userHome;
    shell = pkgs.zsh;
  };

  networking.knownNetworkServices = [
    "Dell Universal Dock D6000"
    "USB 10/100/1000 LAN"
    "VM network interface"
    "USB 10/100 LAN"
    "Dell Universal Hybrid Video Doc"
    "Thunderbolt Bridge"
    "Wi-Fi"
    "Tailscale Tunnel"
  ];

  nix = {
    gc.automatic = true;
    linux-builder = {
      enable = true;
      ephemeral = true;
      config = {
        virtualisation = {
          diskSize = lib.mkForce (1000 * 1024);
        };
      };
      systems = ["aarch64-linux"];
    };
    settings.trusted-users = ["@admin"];
  };

  system.stateVersion = 5;
}
