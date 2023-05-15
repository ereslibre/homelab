{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.2";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        buildInputs = with nixpkgs.legacyPackages.${system}; [
          alejandra
          cachix
        ];
      };
    })
    // {
      # Re-export devenv, home-manager and nixpkgs as usable outputs
      inherit devenv home-manager nixpkgs;
      # Export home-manager configurations
      homeConfigurations = import ./hm-configurations.nix {
        inherit devenv home-manager nixpkgs;
      };
    };
}
