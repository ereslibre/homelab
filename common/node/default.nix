{lib, ...}: {
  imports = [
    ../tailscale
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelParams = ["nohibernate"];
    kernel.sysctl."net.ipv4.ip_forward" = 1;
  };

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  environment.sessionVariables = {
    LIBVIRT_DEFAULT_URI = "qemu:///system";
  };

  networking = {
    useDHCP = lib.mkDefault true;
    firewall.enable = false;
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Madrid";

  users.users.ereslibre.extraGroups = ["libvirtd"];
  virtualisation.libvirtd.enable = true;
}
