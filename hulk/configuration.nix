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
    ../common/kernel
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

  environment.defaultPackages = with pkgs; [nvtopPackages.nvidia];

  networking = {
    firewall.checkReversePath = "loose";
    hostName = "hulk";
  };

  services = {
    ollama = {
      enable = true;
      host = "0.0.0.0";
      # NOTE: LLAMA_ARG_SPEC_TYPE = "draft-mtp" below is global, so EVERY model
      # loaded here must carry an MTP/nextn head. Models without it fail to load
      # with "context type MTP requested but model doesn't contain MTP layers".
      # The Ollama-library qwen3.6:27b lacks the head, so we pull unsloth's
      # MTP GGUF instead; gemma4:31b has no MTP build and was dropped for this.
      loadModels = ["hf.co/unsloth/Qwen3.6-27B-MTP-GGUF:Q5_K_M"];
      package = pkgs.ollama-cuda;
      environmentVariables = {
        CUDA_VISIBLE_DEVICES = "0";
        OLLAMA_CONTEXT_LENGTH = "81920";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";
        # Enable Qwen3.6 multi-token prediction: use the in-model nextn head as
        # the speculative drafter. Ollama forwards LLAMA_ARG_* to its bundled
        # llama-server (--spec-type draft-mtp). Potential ~1.5-2x decode speedup;
        # see the loadModels NOTE above for the all-models-must-have-MTP caveat.
        LLAMA_ARG_SPEC_TYPE = "draft-mtp";
      };
    };
    spice-vdagentd.enable = true;
  };

  systemd.services.ollama = {
    after = ["nvidia-persistenced.service"];
    requires = ["nvidia-persistenced.service"];
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
