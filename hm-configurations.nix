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

  sharedConfiguration = { homeDirectory }: {
    imports = [ ./home.nix ];
    programs = commonConfiguration {
      emacsClient =
        "${nixpkgs.legacyPackages.x86_64-darwin.emacs}/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t";
    };
  };

  macbookConfiguration = { system, username }:
    home-manager.lib.homeManagerConfiguration rec {
      inherit system username;
      homeDirectory = "/Users/${username}";
      configuration = sharedConfiguration { inherit homeDirectory; };
    };

  workstationConfiguration = { system, username }:
    home-manager.lib.homeManagerConfiguration rec {
      inherit system username;
      homeDirectory = "/home/${username}";
      configuration = sharedConfiguration { inherit homeDirectory; };
    };
in {
  "ereslibre@Rafaels-Air" = macbookConfiguration {
    system = "x86_64-darwin";
    username = "ereslibre";
  };
  "ereslibre@cpi5.lab.ereslibre.local" = workstationConfiguration {
    system = "aarch64-linux";
    username = "ereslibre";
  };
}
