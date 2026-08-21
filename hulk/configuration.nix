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
    ../common/netconsole
    ../common/nix
    ../common/nix-github
    ../common/node
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builder
    ../common/sensor-log
    ../common/services
    ../common/users
    ../common/vendor/amd
    ../common/watchdog
  ];

  # Cross-compiling support
  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  boot.tmp = {
    useTmpfs = true;
    tmpfsSize = "20%";
  };

  environment.defaultPackages = with pkgs; [nvtopPackages.nvidia];

  # This box has a failing DIMM. On 2026-07-09 the UMC logged two deferred UECCs
  # on mc#0 csrow#1 channel#4 (pages 0x38d3503 and 0x38d351b), and ~82h later the
  # kernel consumed the poisoned line at that exact first page -- ADDR
  # 38d3503040, in _copy_to_iter -- and took a fatal machine check. A fresh
  # corrected CECC on the same MC17 landed at 2026-08-07 19:24, so the fault is
  # live, and the two silent hangs are very likely the same DIMM.
  #
  # The kernel only reports these to dmesg, which means every reboot loses the
  # history -- the 2026-07-09 evidence survived purely by luck, because that one
  # happened to panic and reach pstore/ERST. rasdaemon persists MCE and EDAC
  # events to sqlite instead, so `ras-mc-ctl --summary` and
  # `ras-mc-ctl --error-count` can build the per-DIMM picture needed to identify
  # which physical slot to pull. Remove this once the DIMM is replaced and the
  # counters have stayed clean for a while.
  hardware.rasdaemon.enable = true;

  networking = {
    firewall.checkReversePath = "loose";
    hostName = "hulk";
  };

  # Bound build concurrency by job count rather than by cores-per-build. Capping
  # cores starved single-derivation builds -- a crane/buildRustPackage crate is
  # one derivation, so cargo only ever got -j8 on a 128-thread box -- while not
  # actually preventing oversubscription, since max-jobs = auto already allowed
  # 128 jobs at once. cores = 0 lets a lone build claim the whole machine, and
  # max-jobs caps the 16 concurrent remote-build slots (see common/remote-builds)
  # at a limit the scheduler enforces for real.
  nix.settings = {
    cores = 0;
    max-jobs = 16;
  };

  services = {
    ollama = {
      enable = true;
      host = "0.0.0.0";
      loadModels = ["hf.co/unsloth/Qwen3.8-27B-GGUF:UD-Q4_K_XL"];
      package = pkgs.ollama-cuda;
      environmentVariables = {
        OLLAMA_CONTEXT_LENGTH = "170000";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";
        OLLAMA_NUM_PARALLEL = "1";
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
