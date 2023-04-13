{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.2";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    home-manager = {
      # Use master until 23.05 is released due to current
      # incompatibilities with nixpkgs/release-22.11 and
      # home-manager/release-22.11. When 23.05 is released, change
      # both
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { devenv, flake-utils, home-manager, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ cachix nix-linter nixfmt ];
        };
      }) // {
        # Re-export devenv, home-manager and nixpkgs as usable outputs
        inherit devenv home-manager nixpkgs;
        # Export home-manager configurations
        homeConfigurations = import ./hm-configurations.nix {
          inherit devenv home-manager nixpkgs;
        };
      };
}
