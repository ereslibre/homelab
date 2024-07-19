{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/nixos
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/users
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  boot = {
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = false;
      raspberryPi = {
        enable = true;
        firmwareConfig = ''
          disable_overscan = 1
        '';
        version = 4;
      };
    };
    kernelParams = ["nohibernate"];
  };

  environment.systemPackages = with pkgs; [firefox];

  networking = {
    hostName = "pi-desktop";
    useDHCP = false;
    networkmanager.enable = true;
  };

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
