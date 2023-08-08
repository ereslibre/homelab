{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.3";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
  };

  outputs = {
    devenv,
    flake-utils,
    home-manager,
    nixpkgs,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [alejandra];
      };
      defaultApp = {
        type = "app";
        program = "${home-manager.packages.${system}.default}/bin/home-manager";
      };
    })
    // {
      # Re-export devenv, flake-utils, home-manager and nixpkgs as usable outputs
      inherit devenv flake-utils home-manager nixpkgs;
      # Export home-manager configurations
      homeConfigurations = import ./hm-configurations.nix {
        inherit devenv home-manager nixpkgs;
      };
    };
}
