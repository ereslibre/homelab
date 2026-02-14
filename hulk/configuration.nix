{
  config,
  nix-ai-tools,
  pkgs,
  ...
}: let
  ai-tools = builtins.map (pkg:
    pkg.overrideAttrs (old: {
      doCheck = false;
      doInstallCheck = false;
    })) (
    with nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}; [
      claude-code
      codex
      copilot-cli
      cursor-agent
      gemini-cli
      goose-cli
      opencode
      qwen-code
    ]
  );
in {
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

  boot.tmp = {
    useTmpfs = true;
    tmpfsSize = "20%";
  };

  environment.defaultPackages =
    ai-tools
    ++ (with pkgs; [
      nvtopPackages.nvidia
    ]);

  environment.systemPackages = ai-tools;

  networking = {
    firewall.checkReversePath = "loose";
    hostName = "hulk";
  };

  services = {
    ollama = {
      enable = true;
      host = "0.0.0.0";
      loadModels = ["gpt-oss:20b" "qwen3-coder-next:latest"];
      environmentVariables = {
        OLLAMA_CONTEXT_LENGTH = "32000";
      };
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
