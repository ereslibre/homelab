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

  sharedConfiguration = { system, homeDirectory }: {
    imports = [ ./home.nix ];
    programs = commonConfiguration {
      emacsClient = "${
          nixpkgs.legacyPackages.${system}.emacs
        }/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t";
    };
    # TODO: check
    home.stateVersion = "20.09";
  };

  macbookRawConfiguration = { system, homeDirectory }: rec {
    configuration = sharedConfiguration { inherit system homeDirectory; };
  };

  macbookConfiguration = { system, username }: rec {
    homeDirectory = "/Users/${username}";
    inherit (macbookRawConfiguration { inherit system homeDirectory; })
      configuration;
    hm-config = home-manager.lib.homeManagerConfiguration {
      inherit system username homeDirectory configuration;
    };
  };

  workstationRawConfiguration = { system, homeDirectory }: {
    configuration = sharedConfiguration { inherit system homeDirectory; };
  };

  workstationConfiguration = { system, username }: rec {
    homeDirectory = "/home/${username}";
    inherit (workstationRawConfiguration { inherit system homeDirectory; })
      configuration;
    hm-config = home-manager.lib.homeManagerConfiguration {
      inherit system username homeDirectory configuration;
    };
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
  "ereslibre@nuc-1.lab.ereslibre.local" = workstationConfiguration {
    system = "x86_64-linux";
    username = "ereslibre";
  };
}
