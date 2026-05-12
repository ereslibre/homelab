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

  # end0 is brought up by the initrd's `ip=dhcp` to mount the iSCSI root.
  # Any userspace DHCP client that "takes over" end0 (NetworkManager or
  # systemd-networkd) bounces the IP enough to break the iSCSI session
  # and hang the kernel at cold boot, so don't let either touch it.
  # Configure DNS statically to the pi-holes instead. Search domains
  # mirror what DHCP would push so unqualified names like `hulk` resolve.
  networking.networkmanager.unmanaged = ["end0"];
  networking.nameservers = ["10.0.4.30" "10.0.4.31"];
  networking.search = ["lab.ereslibre.local" "ereslibre.local" "ereslibre.net"];

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  services.xserver.enable = true;
  services.displayManager = {
    gdm.enable = true;
    autoLogin = {
      enable = true;
      user = "ereslibre";
    };
  };
  services.desktopManager.gnome.enable = true;

  environment.systemPackages = with pkgs; [firefox];

  system.stateVersion = "23.05";
}
