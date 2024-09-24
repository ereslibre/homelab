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
      url = "github:ereslibre/nixos-generators/configure-boot-size";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # FIXME: temporary branch
    nixpkgs.url = "github:ereslibre/nixpkgs/dockerd-rootless-make-etc-var-run-cdi-available";
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
        "Rafaels-Air" = {
          builder = nix-darwin.lib.darwinSystem;
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
            {
              # Resembling: https://github.com/nix-community/nixos-generators/blob/076ea5b672bb1ea535ee84cfdabd0c2f0b7f20c7/formats/raw.nix
              boot = {
                growPartition = true;
                kernelParams = ["console=ttyS0"];
                loader = {
                  grub = {
                    device = "nodev";
                    efiSupport = true;
                    efiInstallAsRemovable = true;
                  };
                  timeout = 0;
                };
                initrd.availableKernelModules = ["uas"];
              };
              fileSystems."/" = {
                device = "/dev/disk/by-label/nixos";
                fsType = "ext4";
              };
              fileSystems."/boot" = {
                device = "/dev/disk/by-label/ESP";
                fsType = "vfat";
              };
            }
            ./devbox/configuration.nix
          ];
        };
        "devbox-qcow" = {
          builder = nixos-generators.nixosGenerate;
          builderArgs = {
            system = "aarch64-linux";
            format = "qcow-efi";
            specialArgs = {
              bootSize = 512;
              additionalSpace = "100GiB";
            };
          };
          system = "aarch64-linux";
          user = "ereslibre";
          host = "devbox";
          modules = [
            {nix.registry.nixpkgs.flake = nixpkgs;}
            {boot.growPartition = true;}
            home-manager.nixosModules.home-manager
            ./devbox/configuration.nix
          ];
        };
        "hulk" = {
          builder = nixpkgs.lib.nixosSystem;
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
        "pi-desktop" = {
          builder = nixpkgs.lib.nixosSystem;
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
