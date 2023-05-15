{
  devenv,
  home-manager,
  nixpkgs,
  stateVersion ? "22.11",
}: let
  programsConfiguration = {emacsClient}: let
    shellExtras = {
      profileExtra = ''
        EDITOR="${emacsClient}"
        if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
        if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
      '';
      sessionVariables = {EDITOR = emacsClient;};
      shellAliases = {emacs = emacsClient;};
    };
  in {
    bash = shellExtras;
    zsh = shellExtras;
  };

  sharedConfiguration = {
    system,
    homeDirectory,
    ...
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
  in [
    {
      programs = programsConfiguration {
        emacsClient =
          if pkgs.stdenv.isDarwin
          then "${pkgs.emacs-nox}/bin/emacsclient -s ${homeDirectory}/.emacs.d/emacs.sock -t"
          else "${pkgs.emacs-nox}/bin/emacsclient -t";
      };
    }
    ./home.nix
    {home.stateVersion = stateVersion;}
  ];

  macbookConfiguration = {
    system,
    username,
    profile,
    hmRaw ? false,
  }:
    machineConfiguration {
      inherit hmRaw;
      homeDirectory = "/Users/${username}";
      inherit system username profile;
    };

  workstationConfiguration = {
    system,
    username,
    profile,
    hmRaw ? false,
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
  in
    machineConfiguration {
      inherit hmRaw;
      homeDirectory = "/home/${username}";
      hmModules = [
        {
          # Enabling linger makes the systemd user services start
          # automatically. In this machine, I want to trigger the
          # `gpg-forward-agent-path` service file automatically as
          # systemd starts, so the socket dir is always created and I
          # can forward the GPG agent through SSH directly without
          # having a first failed connection due to a missing
          # `/run/user/<id>/gnupg`.
          home.activation.linger = home-manager.lib.hm.dag.entryBefore ["reloadSystemd"] ''
            ${pkgs.systemd}/bin/loginctl enable-linger $USER
          '';

          # This service creates the GPG socket dir (`/run/user/<id>/gnupg`) automatically.
          systemd.user.services = {
            "gpg-forward-agent-path" = {
              Unit.Description = "Create GnuPG socket directory";
              Service = {
                ExecStart = "${pkgs.gnupg}/bin/gpgconf --create-socketdir";
                ExecStop = "";
              };
              Install.WantedBy = ["default.target"];
            };
          };

          programs.keychain = {
            keys = [];
            inheritType = "any";
          };

          programs.zsh.shellAliases = {
            gpg = "${pkgs.gnupg}/bin/gpg --no-autostart";
          };
        }
      ];
      inherit system username profile;
    };

  machineConfiguration = {
    system,
    username,
    homeDirectory,
    profile,
    hmModules ? [],
    hmRaw ? false,
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
    extraSpecialArgs = {inherit devenv username profile pkgs;};
    modules =
      hmModules
      ++ (sharedConfiguration {inherit system homeDirectory;})
      ++ [{home = {inherit username homeDirectory stateVersion;};}];
    configuration = {inherit extraSpecialArgs modules;};
  in (
    if hmRaw
    then configuration
    else
      home-manager.lib.homeManagerConfiguration
      (configuration // {inherit pkgs;})
  );

  mapMachineConfigurations = configurations:
    nixpkgs.lib.attrsets.mapAttrs (name: value: {
      config = value.factoryFn value.config;
      rawConfig = value.factoryFn (value.config // {hmRaw = true;});
    })
    configurations;
in
  mapMachineConfigurations {
    "ereslibre@Rafaels-Air" = {
      factoryFn = macbookConfiguration;
      config = {
        system = "x86_64-darwin";
        username = "ereslibre";
        profile = "personal";
      };
    };
    "ereslibre@pi-office" = {
      factoryFn = workstationConfiguration;
      config = {
        system = "aarch64-linux";
        username = "ereslibre";
        profile = "personal";
      };
    };
    "ereslibre@pi-desktop" = {
      factoryFn = workstationConfiguration;
      config = {
        system = "aarch64-linux";
        username = "ereslibre";
        profile = "personal";
      };
    };
    "ereslibre@nuc-1" = {
      factoryFn = workstationConfiguration;
      config = {
        system = "x86_64-linux";
        username = "ereslibre";
        profile = "personal";
      };
    };
    "ereslibre@nuc-2" = {
      factoryFn = workstationConfiguration;
      config = {
        system = "x86_64-linux";
        username = "ereslibre";
        profile = "personal";
      };
    };
    "rfernandezl@rfernandezX6Y3X" = {
      factoryFn = macbookConfiguration;
      config = {
        system = "aarch64-darwin";
        username = "rfernandezl";
        profile = "work";
      };
    };
  }
