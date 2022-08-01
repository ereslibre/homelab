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

  sharedConfiguration = { homeDirectory, system }: {
    imports = [ ./home.nix ];
    programs = commonConfiguration {
      emacsClient = "${
          nixpkgs.legacyPackages.${system}.emacs
        }/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t";
    };
  };

  macbookConfiguration = { system, username }:
    home-manager.lib.homeManagerConfiguration rec {
      inherit system username;
      homeDirectory = "/Users/${username}";
      configuration = sharedConfiguration { inherit homeDirectory system; };
    };

  workstationConfiguration = { system, username }:
    home-manager.lib.homeManagerConfiguration rec {
      inherit system username;
      homeDirectory = "/home/${username}";
      configuration = sharedConfiguration { inherit homeDirectory system; };
    };
in {
  "ereslibre@Rafaels-Air" = macbookConfiguration {
    system = "x86_64-darwin";
    username = "ereslibre";
  };
  "ereslibre@cpi-5.lab.ereslibre.local" = workstationConfiguration {
    system = "aarch64-linux";
    username = "ereslibre";
  };
}
