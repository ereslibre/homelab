{
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # The system *is* the SD image: this module lays out the FAT firmware
    # partition (Pi firmware + U-Boot) and the ext4 root (NIXOS_SD) that
    # carries the full closure, declares the matching fileSystems, and on
    # first boot grows the root partition to fill the card and registers
    # the store paths. No separate install step, no installer media.
    #
    # It already ships the Zero 2 W device tree and a U-Boot that knows
    # the board, and it runs the mainline kernel — so no nixos-hardware
    # module here: that one would swap in the Pi vendor kernel, which is
    # not in the binary cache and would be compiled under emulation.
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  # Wi-Fi is the only network interface on a Zero 2 W; its brcmfmac
  # firmware + NVRAM come from here.
  hardware.enableRedistributableFirmware = true;

  # profiles/base.nix (pulled in by the sd-image module) turns ZFS on by
  # default. Nothing here uses it.
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # 512 MB of RAM and an SD card as the only disk: compressed RAM swap
  # instead of a swap file that would chew through the card. OctoPrint
  # alone idles at ~200 MB, so this is not optional on this board.
  swapDevices = [];
  zramSwap = {
    enable = true;
    memoryPercent = 100;
  };
}
