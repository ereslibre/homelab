{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/docker
    ../common/fonts
    ../common/home-node
    ../common/nix
    ../common/nix-github
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builder
    ../common/services
    ../common/users
    ../common/vendor/amd
    ../common/wyoming
  ];

  # Cross-compiling support
  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  environment.defaultPackages = with pkgs; [
    nvtopPackages.nvidia
  ];

  networking = {
    firewall.checkReversePath = "loose";
    hostName = "hulk";
  };

  services = {
    ollama = {
      enable = true;
      host = "0.0.0.0";
      loadModels = ["gpt-oss:20b"];
    };
    spice-vdagentd.enable = true;
  };

  sops.defaultSopsFile = ./secrets.yaml;

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      runAsRoot = true;
      swtpm.enable = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
