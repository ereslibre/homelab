{modulesPath, ...}: {
  imports = [
    ../hardware-common/filesystems
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usbhid"
      ];
      kernelModules = [];
    };
    kernelModules = ["kvm-intel"];
    kernelParams = ["usbcore.autosuspend=-1"]; # surpillance experiments
    extraModulePackages = [];
  };
}
