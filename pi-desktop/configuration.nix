{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/home-node
    ../common/nix
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/users
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  # No local bootloader — the Pi VideoCore firmware loads kernel + initrd over
  # TFTP from the Synology. Leaving all loaders disabled keeps nixos-rebuild
  # from trying to write to a /boot partition that doesn't exist on the LUN.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = false;

  networking.hostName = "pi-desktop";

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  environment.systemPackages = with pkgs; [firefox];

  system.stateVersion = "23.05";
}
