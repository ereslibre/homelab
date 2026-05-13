# Parameterized per-host module for cpi-N (cluster-pi) headless Pi
# nodes. Each cpi-N is fully described by its integer index `n`:
# hostname, iSCSI initiator IQN, and iSCSI target IQN are all derived.
#
# Used from flake.nix via:
#   mkCpi = n: { modules = [ ... (import ./common/headless-pi/cpi-node.nix { inherit n; }) ]; }
#
# To add a host: append its integer to `cpiNodes` in flake.nix and add
# its MAC to the `MAC_OF` array in scripts/deploy-tftp.sh. No new
# directory or per-host files needed unless the host eventually grows
# its own sops secrets (see "future sops" note at the bottom).
{n}: {
  modulesPath,
  lib,
  ...
}: let
  host = "cpi-${toString n}";
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./.
    ../aliases
    ../nix
    ../packages
    ../programs
    ../services
    ../users
  ];

  networking.hostName = host;

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
    name = "iqn.2026-04.net.ereslibre:${host}";
    # DSM-auto-generated; bound to LUN ${host}-root with an ACL that
    # only accepts the initiator IQN above. The trailing suffix
    # ca49c4149b2 is the Synology chassis identifier, shared across
    # every iSCSI target on this NAS.
    target = "iqn.2000-01.com.synology:synology.${host}.ca49c4149b2";
    discoverPortal = "10.0.4.2";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/PIROOT";
    fsType = "ext4";
  };

  swapDevices = [];

  system.stateVersion = "26.05";

  # Future sops: when this host grows secrets, add
  #   sops.defaultSopsFile = ../../cpi-N/secrets.yaml;
  # in flake.nix's mkCpi (alongside the import of this file), and
  # add the host as &host-cpi-N in .sops.yaml after first-boot rekey.
}
