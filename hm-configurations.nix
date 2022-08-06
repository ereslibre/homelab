{ home-manager, nixpkgs }:
let
  programsConfiguration = { emacsClient }:
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
    imports =
      if (!builtins.pathExists "/etc/NIXOS") then [ ./home.nix ] else null;
    modules =
      if (!builtins.pathExists "/etc/NIXOS") then [ ./home.nix ] else null;
    programs = programsConfiguration {
      emacsClient = if nixpkgs.legacyPackages.${system}.stdenv.isDarwin then
        "${
          nixpkgs.legacyPackages.${system}.emacs
        }/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t"
      else
        "${nixpkgs.legacyPackages.${system}.emacs}/bin/emacsclient -t";
    };
  };

  macbookRawConfiguration = { system, homeDirectory }: {
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

  workstationRawConfiguration = { system, homeDirectory }: rec {
    configuration = (nixpkgs.lib.mkMerge [
      {
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
      }
      (sharedConfiguration { inherit system homeDirectory; })
    ]);
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
  "ereslibre@cpi-5" = workstationConfiguration {
    system = "aarch64-linux";
    username = "ereslibre";
  };
  "ereslibre@cpi-5.lab.ereslibre.local" = workstationConfiguration {
    system = "aarch64-linux";
    username = "ereslibre";
  };
  "ereslibre@nuc-1" = workstationConfiguration {
    system = "x86_64-linux";
    username = "ereslibre";
  };
  "ereslibre@nuc-1.lab.ereslibre.local" = workstationConfiguration {
    system = "x86_64-linux";
    username = "ereslibre";
  };
}
