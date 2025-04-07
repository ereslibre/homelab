{
  description = "Home lab";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    flake-utils,
    home-manager,
    microvm,
    nix-darwin,
    nixos-hardware,
    nixpkgs,
    sops-nix,
    ...
  }: let
    dotfiles = import ./dotfiles;
  in (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      legacyPackages = pkgs;
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [age alejandra just sops ssh-to-age];
      };
    })
    // (let
      mapMachineConfigurations = nixpkgs.lib.mapAttrs (
        host: configuration:
          configuration.builder (
            let
              hmConfiguration =
                dotfiles
                .rawHomeManagerConfigurations
                ."${configuration.user}@${
                  if builtins.hasAttr "host" configuration
                  then configuration.host
                  else host
                }";
            in ({
                inherit (configuration) system;
                modules =
                  configuration.modules
                  ++ [
                    {nixpkgs.config.allowUnfree = true;}
                    {
                      home-manager = {
                        users.${configuration.user} = import ./dotfiles/home.nix {
                          inherit home-manager;
                          inherit (hmConfiguration) system username homeDirectory stateVersion profile mainlyRemote;
                        };
                        useGlobalPkgs = true;
                      };
                    }
                  ];
              }
              // (configuration.builderArgs or {}))
          )
      );
    in {
      darwinConfigurations = mapMachineConfigurations {
        "Rafaels-Flying-Hulk" = {
          builder = nix-darwin.lib.darwinSystem;
          system = "aarch64-darwin";
          user = "ereslibre";
          modules = [
            home-manager.darwinModules.home-manager
            {
              # To be reverted when https://github.com/NixOS/nixpkgs/issues/395169#issuecomment-2769619888 is fixed.
              nixpkgs.overlays = [(final: prev: {emacs = prev.emacs.override {withNativeCompilation = false;};})];
            }
            ./rafaels-flying-hulk/configuration.nix
          ];
        };
      };
      nixosConfigurations = mapMachineConfigurations {
        "hulk" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./hulk/configuration.nix
          ];
        };
        "nuc-1" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-1/configuration.nix
          ];
        };
        "nuc-2" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-2/configuration.nix
          ];
        };
        "nuc-3" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-3/configuration.nix
          ];
        };
      };
    }));
}
