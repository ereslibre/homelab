{
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/home-node
    ../common/nixos
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builder
    ../common/services
    ../common/users
    ../common/vendor/amd
  ];

  # Cross-compiling support
  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  networking.hostName = "hulk";

  sops.defaultSopsFile = ./secrets.yaml;
  nix.extraOptions = ''
    !include ${config.sops.secrets.github-pat.path}
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
