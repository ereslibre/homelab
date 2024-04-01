{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.3";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    devenv,
    flake-utils,
    home-manager,
    nixpkgs,
    ...
  }:
    (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [alejandra just];
      };
      defaultApp = {
        type = "app";
        program = "${home-manager.packages.${system}.default}/bin/home-manager";
      };
    }))
    // (let
      rawHomeManagerConfigurations = {
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
        "ereslibre@pi-desktop" = {
          system = "aarch64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          mainlyRemote = false;
          stateVersion = "22.11";
        };
        "ereslibre@Rafaels-Air" = {
          system = "x86_64-darwin";
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
              inherit system username homeDirectory stateVersion profile mainlyRemote devenv home-manager;
            })
          ];
        };
    in {
      # Export home-manager configurations
      inherit rawHomeManagerConfigurations;
      homeConfigurations =
        nixpkgs.lib.attrsets.mapAttrs
        (userAndHost: userAndHostConfig: homeManagerConfiguration userAndHostConfig)
        rawHomeManagerConfigurations;
    })
    // {
      # Re-export devenv, flake-utils, home-manager and nixpkgs as usable outputs
      inherit devenv flake-utils home-manager nixpkgs;
    };
}
