{
  config,
  modulesPath,
  pkgs,
  ...
}: {
  imports = [
    ../hardware-common/filesystems
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nct6775" # sensors
        "nvme"
        "usbhid"
      ];
      kernelModules = ["xhci_pci" "ahci" "nct6775" "nvme" "usb_storage" "usbhid" "sd_mod"];
    };
    kernelModules = ["kernel-amd"];
    extraModulePackages = [];
  };

  environment = {
    etc."sysconfig/lm_sensors".text = ''
      HWMON_MODULES="nct6775"
    '';
    shellAliases = {
      sensor-cpu = "sudo ${pkgs.lm_sensors}/bin/sensors -j k10temp-pci-00c3 | ${pkgs.jq}/bin/jq '.\"k10temp-pci-00c3\".Tctl.temp1_input'";
    };
    systemPackages = with pkgs; [
      linuxPackages.nvidia_x11
      pciutils
    ];
  };

  fileSystems = {
    "/home" = {
      device = "/dev/disk/by-label/HOME";
      fsType = "ext4";
    };
  };

  hardware = {
    nvidia = {
      modesetting.enable = true;
      nvidiaPersistenced = true;
    };
    opengl.enable = true;
  };

  nixpkgs.config = {
    cudaSupport = true;
    cudnnSupport = true;
  };

  services.xserver.videoDrivers = ["nvidia"];

  systemd.services.nvidia-control-devices = {
    wantedBy = ["multi-user.target"];
    serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  };

  hardware.nvidia-container-toolkit.enable = true;
}
