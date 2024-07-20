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
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
    nixos-generators,
    nixos-hardware,
    nixpkgs,
    sops-nix,
    ...
  }: let
    dotfiles = import ./dotfiles {inherit nixpkgs home-manager;};
  in (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [age alejandra just sops];
      };
    })
    // (let
      mapMachineConfigurations = nixpkgs.lib.mapAttrs (
        host: configuration:
          configuration.builder (
            let
              hmConfiguration = dotfiles.rawHomeManagerConfigurations."${configuration.user}@${host}";
            in
              {
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
              // (
                if configuration.builderArgs == null
                then {}
                else configuration.builderArgs
              )
          )
      );
    in {
      darwinConfigurations = mapMachineConfigurations {
        "Rafaels-Air" = {
          builder = nix-darwin.lib.darwinSystem;
          builderArgs = null;
          system = "aarch64-darwin";
          user = "ereslibre";
          modules = [
            home-manager.darwinModules.home-manager
            ./rafaels-air/configuration.nix
          ];
        };
      };
      nixosConfigurations = mapMachineConfigurations {
        "devbox" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "aarch64-linux";
          user = "ereslibre";
          modules = [
            {nix.registry.nixpkgs.flake = nixpkgs;}
            home-manager.nixosModules.home-manager
            ./devbox/configuration.nix
          ];
        };
        "devbox-qcow" = {
          builder = nixos-generators.nixosGenerate;
          builderArgs = rec {
            system = "aarch64-linux";
            format = "qcow";
            specialArgs.diskSize = "102400";
          };
          system = "aarch64-linux";
          user = "ereslibre";
          modules = [
            {nix.registry.nixpkgs.flake = nixpkgs;}
            home-manager.nixosModules.home-manager
            ./devbox/configuration.nix
          ];
        };
        "hulk" = {
          builder = nixpkgs.lib.nixosSystem;
          builderArgs = null;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            ./hulk/configuration.nix
          ];
        };
        "nuc-1" = {
          builder = nixpkgs.lib.nixosSystem;
          builderArgs = null;
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
          builderArgs = null;
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
          builderArgs = null;
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-3/configuration.nix
          ];
        };
        "pi-desktop" = {
          builder = nixpkgs.lib.nixosSystem;
          builderArgs = null;
          system = "aarch64-linux";
          user = "ereslibre";
          modules = [
            home-manager.nixosModules.home-manager
            nixos-hardware.nixosModules.raspberry-pi-4
            sops-nix.nixosModules.sops
            ./pi-desktop/configuration.nix
          ];
        };
      };
    }));
}
