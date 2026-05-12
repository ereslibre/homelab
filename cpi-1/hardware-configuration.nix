{modulesPath, ...}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  hardware.enableRedistributableFirmware = true;

  # Pi 4 over Ethernet to the Synology iSCSI target.
  # Boot chain:
  #   Pi VideoCore firmware -> TFTP (10.0.4.2) -> kernel + initrd
  #   initrd -> ip=dhcp on end0 -> iscsid logs in -> /dev/sda -> pivot.
  boot.initrd.availableKernelModules = [
    "usbhid"
    "iscsi_tcp"
    "crc32c"
    "libcrc32c"
  ];
  boot.initrd.kernelModules = [];
  boot.initrd.network.enable = true;

  # iscsi-initiator still requires the legacy bash-script initrd; modern
  # nixpkgs defaults systemd-stage-1 to true and that path doesn't
  # support iSCSI yet. Same constraint as pi-desktop.
  boot.initrd.systemd.enable = false;

  boot.iscsi-initiator = {
    name = "iqn.2026-04.net.ereslibre:cpi-1";
    # DSM-auto-generated target IQN, bound to the LUN with an ACL that
    # only accepts the initiator IQN above. Update this string once the
    # LUN+target are created in DSM SAN Manager.
    target = "iqn.2000-01.com.synology:synology.target.TBD-cpi-1";
    discoverPortal = "10.0.4.2";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/PIROOT";
    fsType = "ext4";
  };

  swapDevices = [];
}
