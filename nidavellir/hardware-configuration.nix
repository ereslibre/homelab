{
  lib,
  modulesPath,
  nixos-raspberrypi,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # The system *is* the SD image: this module lays out the FAT firmware
    # partition (Pi firmware + U-Boot) and the ext4 root (NIXOS_SD) that
    # carries the full closure, declares the matching fileSystems, and on
    # first boot grows the root partition to fill the card and registers
    # the store paths. No separate install step, no installer media.
    nixos-raspberrypi.nixosModules.sd-image
  ];

  hardware.enableRedistributableFirmware = true;

  # profiles/base.nix (pulled in by the sd-image module) turns ZFS on by
  # default, which would mean compiling the module against the Pi vendor
  # kernel under emulation. Nothing here uses it.
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # 512 MB Pi with an SD card as its only disk: compressed RAM swap, sized
  # to the whole of RAM, instead of a swap file that would chew through
  # the card.
  swapDevices = [];
  zramSwap = {
    enable = true;
    memoryPercent = 100;
  };
}
