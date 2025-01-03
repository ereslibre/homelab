{
  nixpkgs,
  home-manager,
}: let
  rawHomeManagerConfigurations = {
    "ereslibre@devbox" = {
      system = "aarch64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      stateVersion = "24.05";
    };
    "ereslibre@hulk" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "23.05";
    };
    "ereslibre@nuc-1" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-2" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-3" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "23.05";
    };
    "ereslibre@Rafaels-Air" = {
      system = "aarch64-darwin";
      username = "ereslibre";
      homeDirectory = "/Users/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      stateVersion = "22.11";
    };
  };

  homeManagerConfiguration = {
    system,
    username,
    homeDirectory,
    profile,
    mainlyRemote,
    stateVersion,
  }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
      };
      modules = [
        {nixpkgs.config.allowUnfree = true;}
        (import ./home.nix {
          inherit system username homeDirectory stateVersion profile mainlyRemote home-manager;
        })
      ];
    };
in {
  inherit rawHomeManagerConfigurations;
  homeConfigurations =
    nixpkgs.lib.attrsets.mapAttrs
    (userAndHost: userAndHostConfig: homeManagerConfiguration userAndHostConfig)
    rawHomeManagerConfigurations;
}
