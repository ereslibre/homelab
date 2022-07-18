{ home-manager, nixpkgs }:
let
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
in { "ereslibre@Rafaels-Air" = macbookConfiguration; }
