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

      homeManagerConfigurations = {
        "ereslibre@hulk" = {
          system = "x86_64-linux";
          username = "ereslibre";
          host = "hulk";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "23.05";
        };
        "ereslibre@nuc-1" = {
          system = "x86_64-linux";
          username = "ereslibre";
          host = "nuc-1";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@nuc-2" = {
          system = "x86_64-linux";
          username = "ereslibre";
          host = "nuc-2";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@nuc-3" = {
          system = "x86_64-linux";
          username = "ereslibre";
          host = "nuc-3";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "23.05";
        };
        "ereslibre@pi-desktop" = {
          system = "aarch64-linux";
          username = "ereslibre";
          host = "pi-desktop";
          homeDirectory = "/home/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "ereslibre@Rafaels-Air" = {
          system = "x86_64-darwin";
          username = "ereslibre";
          host = "Rafaels-Air";
          homeDirectory = "/Users/ereslibre";
          profile = "personal";
          stateVersion = "22.11";
        };
        "rfernandezl@rfernandezX6Y3X" = {
          system = "aarch64-darwin";
          username = "rfernandezl";
          host = "rfernandezX6Y3X";
          homeDirectory = "/Users/rfernandezl";
          profile = "work";
          stateVersion = "22.11";
        };
      };

      homeManagerConfiguration = {
        system,
        username,
        host,
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
      inherit homeManagerConfigurations;
      homeConfigurations =
        nixpkgs.lib.attrsets.mapAttrs
        (userAndHost: userAndHostConfig: homeManagerConfiguration userAndHostConfig)
        homeManagerConfigurations;
    })
    // {
      # Re-export devenv, flake-utils, home-manager and nixpkgs as usable outputs
      inherit devenv flake-utils home-manager nixpkgs;
    };
}
