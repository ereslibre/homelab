{
  lib,
  modulesPath,
  nixos-raspberrypi,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # The system *is* the SD image: this module lays out the FAT firmware
    # partition (Pi firmware, config.txt, kernels) and the ext4 root
    # (NIXOS_SD) that carries the full closure, declares the matching
    # fileSystems, and on first boot grows the root partition to fill the
    # card and registers the store paths. No separate install step, no
    # installer media. It is nixos-raspberrypi's take on nixpkgs'
    # sd-image-aarch64.nix, which cannot boot a Pi 5.
    nixos-raspberrypi.nixosModules.sd-image
  ];

  hardware.enableRedistributableFirmware = true;
  hardware.graphics.enable = true;

  # profiles/base.nix (pulled in by the sd-image module) turns ZFS on by
  # default, which would mean compiling the module against the Pi vendor
  # kernel under emulation. Nothing here uses it.
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # SD card as the only disk: compressed RAM swap instead of a swap file
  # that would chew through the card.
  swapDevices = [];
  zramSwap.enable = true;
}
