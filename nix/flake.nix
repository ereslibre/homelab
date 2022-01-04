{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, home-manager }: {
    defaultApp.x86_64-darwin = flake-utils.lib.mkApp { drv = nixpkgs.legacyPackages.x86_64-darwin.home-manager; };

    homeConfigurations = {
      "ereslibre@Rafaels-MacBook-Air" = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-darwin";
        homeDirectory = "/Users/ereslibre";
        username = "ereslibre";
        configuration.imports = [ ./home.nix ];
      };
      "ereslibre@desktop" = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/ereslibre";
        username = "ereslibre";
        configuration.imports = [ ./home.nix ];
      };
    };
  };
}
