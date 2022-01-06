{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, ... }: {
    homeConfigurations = let
      macbookConfiguration = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-darwin";
        homeDirectory = "/Users/ereslibre";
        username = "ereslibre";
        configuration.imports = [ ./home.nix ];
      };
      desktopConfiguration = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/ereslibre";
        username = "ereslibre";
        configuration = {
          imports = [ ./home.nix ];
          programs.keychain = {
            keys = [ ];
            inheritType = "any";
          };
          # Bash is required because is the hardcoded `runtimeShell`
          # used by the emacs service user unit:
          # https://github.com/nix-community/home-manager/blob/c751aeb19e84a0a777f36fd5ea73482a066bb406/modules/services/emacs.nix#L116.
          #
          # This is so the emacs service is started in a login shell
          # that sources the PATH and the nix profile.
          #
          # It would be nice to generalize that and remove the hardcoded need for bash, coming from nixpkgs -- all-packages.nix --:
          #
          # ```nix
          # runtimeShell = "${runtimeShellPackage}${runtimeShellPackage.shellPath}";
          # runtimeShellPackage = bash;
          # ```
          programs.bash = {
            enable = true;
            profileExtra = ''
              if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
              if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
            '';
            initExtra = ''
              exec ''${HOME}/.nix-profile/bin/zsh
            '';
          };
          services.emacs.enable = true;
        };
      };
    in {
      "ereslibre@Rafaels-Air" = macbookConfiguration;
      "ereslibre@Rafaels-MacBook-Air" = macbookConfiguration;
      "ereslibre@desktop" = desktopConfiguration;
    };
  };
}
