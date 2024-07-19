{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../common/nixos
  ];

  environment = {
    shells = with pkgs; [zsh];
    systemPackages = with pkgs; [tailscale];
  };

  fonts.packages = with pkgs; [fira-code];

  programs.zsh.enable = true;

  users.users.ereslibre = {
    createHome = true;
    home = "/Users/ereslibre";
    shell = pkgs.zsh;
  };

  networking.knownNetworkServices = [
    "Dell Universal Dock D6000"
    "USB 10/100/1000 LAN"
    "VM network interface"
    "USB 10/100 LAN"
    "Dell Universal Hybrid Video Doc"
    "Thunderbolt Bridge"
    "Wi-Fi"
    "Tailscale Tunnel"
  ];

  nix = {
    gc.automatic = true;
    linux-builder = {
      enable = true;
      ephemeral = true;
      config = {
        virtualisation = {
          diskSize = lib.mkForce (1000 * 1024);
        };
      };
    };
    settings.trusted-users = ["@admin"];
  };

  services = {
    nix-daemon.enable = true;
    tailscale = {
      enable = true;
      overrideLocalDns = true;
    };
  };
}
