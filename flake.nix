{
  description = "Home Sweet Home";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    # nixpkgs revision known to work with Raspberry Pi 4b/400
    nixpkgs-rpi.url =
      "github:nixos/nixpkgs/c71f061c68ba8ce53471b767d5049cbd0f3d8490";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-rpi = {
      url = "github:nix-community/home-manager/release-22.05";
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
        homeConfigurations = import ./hm-configurations.nix {
          inherit home-manager nixpkgs;
          stateVersion = "22.05";
        };
        homeConfigurationsRpi = import ./hm-configurations.nix {
          home-manager = home-manager-rpi;
          nixpkgs = nixpkgs-rpi;
          stateVersion = "22.05";
        };
      };
}
