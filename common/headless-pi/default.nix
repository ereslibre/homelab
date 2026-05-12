# Baseline for headless aarch64 Pi 4 nodes that netboot over TFTP and
# mount root from an iSCSI LUN on the Synology. Mirrors what we learned
# bringing up pi-desktop, minus the display chain.
#
# Each host that imports this still needs to declare its own
# hardware-configuration.nix (iSCSI initiator IQN + target, root mount)
# and a minimal configuration.nix with hostname + sops paths.
{
  # No local bootloader — kernel + initrd come over TFTP from the Synology
  # (see pi-desktop/README.md for the full architecture).
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = false;

  # end0 is brought up by the initrd's `ip=dhcp` to mount the iSCSI root.
  # Any userspace DHCP client that "takes over" end0 (NetworkManager or
  # systemd-networkd) bounces the IP enough to break the iSCSI session
  # and hang the kernel at cold boot. Keep all userspace DHCP off and
  # configure DNS statically to the pi-holes — the router's DHCP would
  # push the same two servers and search-domain set anyway.
  networking.useDHCP = false;
  networking.nameservers = ["10.0.4.30" "10.0.4.31"];
  networking.search = ["lab.ereslibre.local" "ereslibre.local" "ereslibre.net"];

  # Minimal stack appropriate for a headless rack node. No display
  # manager, no Wayland, no vc4 dtoverlay in TFTP's config.txt.
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Madrid";
}
