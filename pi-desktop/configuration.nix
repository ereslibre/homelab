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

  # The initrd brings end0 up with `ip=dhcp` to mount the iSCSI root. After
  # pivot NetworkManager finds the interface already configured and marks it
  # "connected (externally)" — without a profile of its own, NM never re-DHCPs
  # and per-link DNS / search-domain info from the lease is never applied.
  # This explicit profile makes NM own end0, refresh the lease, and pass DNS
  # to systemd-resolved.
  networking.networkmanager.ensureProfiles.profiles."end0" = {
    connection = {
      id = "end0";
      type = "ethernet";
      interface-name = "end0";
      autoconnect = true;
    };
    ipv4.method = "auto";
    ipv6.method = "auto";
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
