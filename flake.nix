{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.6.2";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/master";
    home-manager = {
      # Use master, given it is the only branch that contains
      # f69816489d5bcd1329c50fb4a7035a9a9dc19a3b. This commit is
      # necessary for dotfiles pass CI, because it creates missing
      # directories that are not longer created by Nix 2.14.
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
