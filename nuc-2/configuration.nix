{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/mdns-reflector
    ../common/home-node
    ../common/nix
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/users
    ../common/vendor/intel
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  sops.secrets.k3s-token = {
    restartUnits = ["k3s.service"];
  };

  boot.kernelParams = [
    # On this machine, at certain time after startup, dmesg says:
    #   [  144.383019] irq 16: nobody cared (try booting with the "irqpoll" option)
    "irqpoll"
    "nohibernate"
  ];

  networking.hostName = "nuc-2";

  services.k3s = {
    enable = true;
    role = "agent";
    extraFlags = toString [
      "--node-ip"
      "10.0.4.31"
    ];
    serverAddr = "https://nuc-1.ereslibre.net:6443";
    tokenFile = config.sops.secrets.k3s-token.path;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
