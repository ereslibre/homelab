{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.3";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
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
        buildInputs = with pkgs; [alejandra];
      };
      defaultApp = {
        type = "app";
        program = "${home-manager.packages.${system}.default}/bin/home-manager";
      };
    }))
    // (let
      homeManagerModules = {
        system,
        username,
        homeDirectory,
        profile,
        stateVersion,
      }: let
        pkgs = nixpkgs.legacyPackages.${system};
      in [
        (import ./home.nix {
          inherit username homeDirectory stateVersion pkgs nixpkgs profile devenv home-manager;
        })
      ];

      rawHomeManagerConfigurations = {
        "ereslibre@hulk" = {
          system = "x86_64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "23.05";
        };
        "ereslibre@nuc-1" = {
          system = "x86_64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@nuc-2" = {
          system = "x86_64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@nuc-3" = {
          system = "x86_64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "23.05";
        };
        "ereslibre@pi-desktop" = {
          system = "aarch64-linux";
          username = "ereslibre";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@Rafaels-Air" = {
          system = "x86_64-darwin";
          username = "ereslibre";
          homeDirectory = "/Users/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "rfernandezl@rfernandezX6Y3X.vmware.com" = {
          system = "aarch64-darwin";
          username = "rfernandezl";
          homeDirectory = "/Users/rfernandezl";
          profile = "work";
          stateVersion = "22.11";
        };
      };

      homeManagerConfiguration = {
        system,
        username,
        homeDirectory,
        profile,
        stateVersion,
      }: (let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = homeManagerModules {inherit system username homeDirectory profile stateVersion;};
        });
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
