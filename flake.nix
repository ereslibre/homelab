{
  description = "Home Sweet Home";

  inputs = {
    devenv.url = "github:cachix/devenv/v0.5";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    home-manager = {
      # TODO: go back to release-22.11 when
      # https://github.com/nix-community/home-manager/issues/3516 is
      # merged. CI does not pass due to using ambient darwin readlink
      # binary.
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { devenv, flake-utils, home-manager, nixpkgs, ... }:
    flake-utils.lib.eachSystem
    (flake-utils.lib.defaultSystems ++ [ "aarch64-darwin" ]) (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ cachix nix-linter nixfmt ];
        };
      }) // {
        # Re-export home-manager and nixpkgs as a usable output
        inherit home-manager nixpkgs;
        # Export home-manager configurations
        homeConfigurations = import ./hm-configurations.nix {
          inherit devenv home-manager nixpkgs;
        };
      };
}
