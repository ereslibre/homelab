{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, ... }: {
    homeConfigurations = let
      commonConfiguration = { emacsClient }:
        let
          shellExtras = {
            profileExtra = ''
              if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
              if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
            '';
            sessionVariables = { EDITOR = emacsClient; };
            shellAliases = { emacs = emacsClient; };
          };
        in {
          bash = shellExtras;
          zsh = shellExtras;
        };
      macbookSharedConfiguration = homeDirectory: {
        imports = [ ./home.nix ];
        programs = commonConfiguration {
          emacsClient =
            "${nixpkgs.legacyPackages.x86_64-darwin.emacs}/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t";
        };
      };
      macbookConfiguration = home-manager.lib.homeManagerConfiguration
        (let homeDirectory = "/Users/ereslibre";
        in {
          system = "x86_64-darwin";
          inherit homeDirectory;
          username = "ereslibre";
          configuration = macbookSharedConfiguration homeDirectory;
        });
      macbookProConfiguration = home-manager.lib.homeManagerConfiguration
        (let homeDirectory = "/Users/ereslibre";
        in {
          system = "aarch64-darwin";
          inherit homeDirectory;
          username = "ereslibre";
          configuration = macbookSharedConfiguration homeDirectory;
        });
      desktopConfiguration = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/ereslibre";
        username = "ereslibre";
        configuration = {
          imports = [ ./home.nix ];
          # Enabling linger makes the systemd user services start
          # automatically. In this machine, I want to trigger the
          # `gpg-forward-agent-path` service file automatically as
          # systemd starts, so the socket dir is always created and I
          # can forward the GPG agent through SSH directly without
          # having a first failed connection due to a missing
          # `/run/user/<id>/gnupg`.
          home.activation.linger =
            home-manager.lib.hm.dag.entryBefore [ "reloadSystemd" ] ''
              loginctl enable-linger $USER
            '';
          # This service creates the GPG socket dir (`/run/user/<id>/gnupg`) automatically.
          systemd.user.services = {
            "gpg-forward-agent-path" = {
              Unit.Description = "Create GnuPG socket directory";
              Service = {
                ExecStart =
                  "${nixpkgs.legacyPackages.x86_64-linux.gnupg}/bin/gpgconf --create-socketdir";
                ExecStop = "";
              };
              Install.WantedBy = [ "default.target" ];
            };
          };
          programs = nixpkgs.lib.recursiveUpdate (commonConfiguration {
            emacsClient =
              "${nixpkgs.legacyPackages.x86_64-linux.emacs}/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs/server -t";
          }) {
            keychain = {
              keys = [ ];
              inheritType = "any";
            };
            zsh = {
              shellAliases = {
                gpg =
                  "${nixpkgs.legacyPackages.x86_64-linux.gnupg}/bin/gpg --no-autostart";
              };
            };
          };
          services.emacs = {
            enable = true;
            socketActivation.enable = true;
          };
        };
      };
    in {
      "ereslibre@Rafaels-Air" = macbookConfiguration;
      "ereslibre@MacBook-Air" = macbookConfiguration;
      "ereslibre@Rafaels-MacBook-Air" = macbookConfiguration;
      "ereslibre@desktop" = desktopConfiguration;
      "ereslibre@Rafaels-Pro" = macbookProConfiguration;
    };
  };
}
