{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/mdns-reflector
    ../common/home-node
    ../common/network-ingress
    ../common/nixos
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/tailscale
    ../common/users
    ../common/vendor/intel
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  sops.secrets.k3s-token = {
    restartUnits = ["k3s.service"];
  };

  networking = {
    hostName = "nuc-1";
    nat.internalInterfaces = ["enp2s0"];
  };

  services.k3s = {
    enable = true;
    role = "server";
    tokenFile = config.sops.secrets.k3s-token.path;
    extraFlags = toString [
      "--disable"
      "traefik"
      "--node-ip"
      "10.0.4.30"
      "--tls-san"
      "nuc-1.ereslibre.net"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
