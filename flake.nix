{
  description = "Home Sweet Home";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    # nixpkgs revision known to work -- https://channels.nix.gsc.io
    nixpkgs-rpi.url =
      "github:nixos/nixpkgs/72f492e275fc29d44b3a4daf952fbeffc4aed5b8";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-rpi = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs-rpi";
    };
  };

  outputs =
    { flake-utils, home-manager, home-manager-rpi, nixpkgs, nixpkgs-rpi, ... }:
    flake-utils.lib.eachSystem
    (flake-utils.lib.defaultSystems ++ [ "aarch64-darwin" ]) (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ cachix nix-linter nixfmt ];
        };
      }) // {
        # Re-export home-manager, home-manager-rpi, nixpkgs and nixpkgs-rpi as a usable output
        inherit home-manager home-manager-rpi nixpkgs nixpkgs-rpi;
        # Export home-manager configurations
        homeConfigurations =
          import ./hm-configurations.nix { inherit home-manager nixpkgs; };
        homeConfigurationsRpi = import ./hm-configurations.nix {
          home-manager = home-manager-rpi;
          nixpkgs = nixpkgs-rpi;
        };
      };
}
