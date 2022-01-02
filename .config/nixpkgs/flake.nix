{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }: {
    defaultApp.x86_64-darwin = {
      type = "app";
      program = "${nixpkgs.legacyPackages.x86_64-darwin.home-manager}/bin/home-manager";
    };

    homeConfigurations = {
      "ereslibre@Rafaels-MacBook-Air.local" = self.inputs.home-manager.lib.homeManagerConfiguration {
        # system = "x86_64-darwin";
        # homeDirectory = "/Users/ereslibre";
        # username = "ereslibre";
        # configuration = { config, pkgs, ... }: {
        #   imports = [ ./common.nix ];
        # };
      };
    };
  };
}
