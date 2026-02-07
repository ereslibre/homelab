{
  config,
  pkgs,
  lib,
  ...
}: let
  user = "ereslibre";
  userHome = "/Users/${user}";
  hmEmacsPackage = config.home-manager.users.${user}.programs.emacs.finalPackage;
in {
  imports = [
    ../common/nix
    ../common/tailscale
  ];

  system.primaryUser = "ereslibre";

  nixpkgs.overlays = [
    # To be removed when https://github.com/NixOS/nixpkgs/issues/395169#issuecomment-2769619888 is fixed.
    (final: prev: {emacs = prev.emacs.override {withNativeCompilation = false;};})
    # libvirt python bindings try to access /private/etc/ssl/openssl.cnf during
    # import check, which is blocked by the Nix sandbox on macOS.
    (final: prev: {
      python3 = prev.python3.override {
        packageOverrides = pyFinal: pyPrev: {
          libvirt = pyPrev.libvirt.overridePythonAttrs (old: {
            doCheck = false;
            pythonImportsCheck = [];
          });
        };
      };
      python3Packages = final.python3.pkgs;
    })
  ];

  environment = {
    shells = with pkgs; [zsh];
    systemPackages = with pkgs; [virt-manager];
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
              <string>${lib.getExe hmEmacsPackage}</string>
              <string>--fg-daemon</string>
            </array>
            <key>RunAtLoad</key>
            <true/>
            <key>KeepAlive</key>
            <true/>
            <key>LSUIElement</key>
            <true/>
            <key>StandardErrorPath</key>
            <string>/tmp/emacs.err</string>
            <key>StandardOutPath</key>
            <string>/tmp/emacs.out</string>
          </dict>
          </plist>
        '';
      };
    };
    variables = {
      GSETTINGS_BACKEND = "keyfile";
    };
  };

  programs.zsh.enable = true;

  home-manager.users.${user} = {
    home = {
      packages = with pkgs; [ollama];
      sessionVariables = {
        OLLAMA_HOST = "hulk.ereslibre.net";
      };
    };

    programs.keychain = {
      enable = true;
      enableZshIntegration = true;
      extraFlags = ["--ignore-missing" "--quiet"];
      keys = ["id_ed25519" "id_rsa"];
    };
  };

  users.users.${user} = {
    createHome = true;
    home = userHome;
    shell = pkgs.zsh;
  };

  networking.search = ["ereslibre.net" "ereslibre.local" "pirate-salak.ts.net"];

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

  ids.gids.nixbld = 350;

  nix = {
    gc.automatic = true;
    linux-builder = {
      config = {
        virtualisation = {
          cores = 6;
          darwin-builder = {
            diskSize = 100 * 1024;
            memorySize = 6 * 1024;
          };
        };
      };
      enable = true;
      ephemeral = true;
      systems = ["x86_64-linux" "aarch64-linux"];
    };
    settings.trusted-users = ["@admin"];
  };

  services.tailscale.overrideLocalDns = true;

  system.stateVersion = 6;
}
