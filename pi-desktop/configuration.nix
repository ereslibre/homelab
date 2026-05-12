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
  # Don't let NetworkManager touch it post-pivot — its takeover bounces
  # the IP, which kills the iSCSI session and freezes the system. Have
  # systemd-networkd manage end0 instead: its default KeepConfiguration
  # is "dynamic", which preserves the externally-set IP and just refreshes
  # the DHCP lease in place so DNS / search-domain / routes land in
  # systemd-resolved per-link.
  networking.networkmanager.unmanaged = ["end0"];
  systemd.network = {
    enable = true;
    networks."10-end0" = {
      matchConfig.Name = "end0";
      networkConfig.DHCP = "yes";
      dhcpV4Config = {
        UseDNS = true;
        UseDomains = true;
        UseRoutes = true;
      };
    };
  };

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
