{
  description = "Home lab";

  nixConfig = {
    extra-substituters = ["https://cache.numtide.com" "https://cache.garnix.io"];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-ai-tools = {
      url = "github:numtide/nix-ai-tools";
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
    emacs-overlay,
    flake-utils,
    home-manager,
    microvm,
    nix-ai-tools,
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
        overlays = [emacs-overlay.overlays.default];
      };
    in {
      inherit nixpkgs;
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
                    {nixpkgs.overlays = [emacs-overlay.overlays.default];}
                    {
                      home-manager = {
                        users.${configuration.user} = import ./dotfiles/home.nix {
                          inherit home-manager;
                          inherit (hmConfiguration) system username homeDirectory stateVersion profile mainlyRemote;
                        };
                        useGlobalPkgs = true;
                        extraSpecialArgs = {
                          inherit emacs-overlay;
                        };
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
            ./rafaels-flying-hulk/configuration.nix
          ];
        };
      };
      nixosConfigurations = mapMachineConfigurations {
        "hulk" = {
          builder = nixpkgs.lib.nixosSystem;
          system = "x86_64-linux";
          user = "ereslibre";
          builderArgs = {
            specialArgs = {
              inherit nix-ai-tools;
            };
          };
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
          builderArgs = {
            specialArgs = {
              inherit sops-nix;
            };
          };
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
