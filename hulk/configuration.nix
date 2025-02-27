{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/docker
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
  sops.secrets."nix-access-tokens" = {
    owner = "ereslibre";
    group = "users";
  };
  nix.extraOptions = ''
    # Access tokens
    !include ${config.sops.secrets.nix-access-tokens.path}
  '';

  services.ollama = {
    enable = true;
    host = "0.0.0.0";
    loadModels = ["deepseek-r1:32b" "deepseek-coder-v2:16b" "llama3.2:3b" "qwen2.5:7b" "qwen2.5-coder:32b"];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
