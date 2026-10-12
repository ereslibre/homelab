{
  lib,
  modulesPath,
  pkgs,
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
    # It ships the Pi 5 device trees and a U-Boot that knows the board,
    # so no nixos-hardware module here: that one would swap in the Pi
    # vendor kernel, which is not in the binary cache and would be
    # compiled under emulation.
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  # Mainline, and newer than the default LTS: the Pi 5's GPU (v3d) only
  # got its device tree node after 6.18, and without it cage and Chromium
  # render in software.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # The SD slot and RP1, the Pi 5's I/O chip behind PCIe that carries
  # USB (the touch panel, a keyboard) and Ethernet.
  boot.initrd.availableKernelModules = [
    "sdhci-brcmstb"
    "pcie-brcmstb"
    "rp1_pci"
    "clk-rp1"
    "pinctrl-rp1"
  ];

  # Wi-Fi is the only network interface in use; its brcmfmac firmware +
  # NVRAM come from here.
  hardware.enableRedistributableFirmware = true;
  hardware.graphics.enable = true;

  # profiles/base.nix (pulled in by the sd-image module) turns ZFS on by
  # default. Nothing here uses it.
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # SD card as the only disk: compressed RAM swap instead of a swap file
  # that would chew through the card.
  swapDevices = [];
  zramSwap.enable = true;
}
