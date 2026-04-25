{modulesPath, ...}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  hardware.enableRedistributableFirmware = true;

  # No SD, no USB disk, no eMMC — the only storage this system ever mounts
  # lives on the Synology iSCSI LUN. The boot chain is:
  #   Pi VideoCore firmware -> TFTP (10.0.4.2) -> kernel + initrd
  #   initrd -> DHCP on end0 -> iscsid logs in -> /dev/sda -> pivot root.
  boot.initrd.availableKernelModules = [
    "usbhid"
    "iscsi_tcp"
    "crc32c"
    "libcrc32c"
  ];
  boot.initrd.kernelModules = [];
  boot.initrd.network.enable = true;
  # iscsi-initiator requires the legacy bash-script initrd; modern nixpkgs
  # defaults systemd-stage-1 to true and that path doesn't support iSCSI yet.
  boot.initrd.systemd.enable = false;

  boot.iscsi-initiator = {
    name = "iqn.2026-04.net.ereslibre:pi-desktop";
    target = "iqn.2000-01.com.synology:synology.default-target.ca49c4149b2";
    discoverPortal = "10.0.4.2";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/PIROOT";
    fsType = "ext4";
  };

  swapDevices = [];

  hardware.bluetooth.enable = true;
}
