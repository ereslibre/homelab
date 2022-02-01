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
      macbookConfiguration = home-manager.lib.homeManagerConfiguration
        (let homeDirectory = "/Users/ereslibre";
        in {
          system = "x86_64-darwin";
          inherit homeDirectory;
          username = "ereslibre";
          configuration = {
            imports = [ ./home.nix ];
            programs.zsh = let
              emacsClient =
                "${nixpkgs.legacyPackages.x86_64-darwin.emacs}/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t";
            in {
              envExtra = ''
                export EDITOR="${emacsClient}"
              '';
              shellAliases = { emacs = emacsClient; };
            };
          };
        });
      desktopConfiguration = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/ereslibre";
        username = "ereslibre";
        configuration = {
          imports = [ ./home.nix ];
          home = {
            file = {
              ".config/systemd/user/gpg-forward-agent-path.service".source =
                ./assets/gpg/gpg-forward-agent-path.service;
            };
          };
          programs = {
            keychain = {
              keys = [ ];
              inheritType = "any";
            };
            bash = {
              profileExtra = ''
                if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
                if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
              '';
            };
            zsh = let
              emacsClient =
                "${nixpkgs.legacyPackages.x86_64-linux.emacs}/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs/server -t";
            in {
              envExtra = ''
                export EDITOR="${emacsClient}"
              '';
              profileExtra = ''
                if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
                if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
              '';
              shellAliases = {
                emacs = emacsClient;
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
    };
  };
}
