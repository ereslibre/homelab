{
  description = "Home lab";

  inputs = {
    #nixpkgs.url = "github:ereslibre/nixpkgs/cdi-add-nvidia-docker-1-directories";
    nixpkgs.url = "git+file:///home/ereslibre/projects/NixOS/nixpkgs";
    dotfiles = {
      url = "github:ereslibre/dotfiles";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    dotfiles,
    microvm,
    nixos-hardware,
    sops-nix,
    ...
  }: let
    flake-utils = dotfiles.flake-utils;
    home-manager = dotfiles.home-manager;
    nixpkgs = dotfiles.nixpkgs;
  in (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [age alejandra just sops];
      };
    })
    // (let
      mapMachineConfigurations = nixpkgs.lib.mapAttrs (host: configuration:
        nixpkgs.lib.nixosSystem (
          let
            hmConfiguration = dotfiles.rawHomeManagerConfigurations."${configuration.user}@${host}";
          in {
            inherit (configuration) system;
            modules =
              configuration.modules
              ++ [
                {nixpkgs.config.allowUnfree = true;}
                home-manager.nixosModules.home-manager
                {
                  home-manager = {
                    users.${configuration.user} = import "${dotfiles}/home.nix" {
                      inherit (dotfiles) devenv home-manager;
                      inherit (hmConfiguration) system username homeDirectory stateVersion profile mainlyRemote;
                    };
                    useGlobalPkgs = true;
                  };
                }
              ];
          }
        ));
    in {
      nixosConfigurations = mapMachineConfigurations {
        "hulk" = {
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            microvm.nixosModules.host
            ./hulk/configuration.nix
          ];
        };
        "nuc-1" = {
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-1/configuration.nix
          ];
        };
        "nuc-2" = {
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            microvm.nixosModules.host
            sops-nix.nixosModules.sops
            ./nuc-2/configuration.nix
          ];
        };
        "nuc-3" = {
          system = "x86_64-linux";
          user = "ereslibre";
          modules = [
            microvm.nixosModules.host
            ./nuc-3/configuration.nix
          ];
        };
        "pi-desktop" = {
          system = "aarch64-linux";
          user = "ereslibre";
          modules = [
            nixos-hardware.nixosModules.raspberry-pi-4
            ./pi-desktop/configuration.nix
          ];
        };
      };
    }));
}
