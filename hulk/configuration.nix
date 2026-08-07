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
    ../common/watchdog
    # TEMP(2026-07-30): disabled to avoid compiling CUDA onnxruntime 1.27.1 from
    # source (faster-whisper + piper both pull it; not in any substituter for the
    # current nixpkgs rev). Re-enable once cached or building in a cooler room.
    # ../common/wyoming
  ];

  # Cross-compiling support
  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  boot.tmp = {
    useTmpfs = true;
    tmpfsSize = "20%";
  };

  nixpkgs.overlays = [
    (final: prev: {
      # Ollama exposes no way to pass extra flags to the llama-server it spawns,
      # so wrap the bundled binary to always start with MTP speculative decoding.
      # Same all-models-must-have-MTP caveat as the old LLAMA_ARG_SPEC_TYPE env
      # var; this just makes the flag explicit in argv instead of relying on
      # Ollama forwarding its environment to the child process.
      ollama-cuda = prev.ollama-cuda.overrideAttrs (old: {
        postFixup =
          (old.postFixup or "")
          + ''
            real="$out/lib/ollama/.llama-server-wrapped"
            mv "$out/lib/ollama/llama-server" "$real"
            echo '#!${prev.runtimeShell}' > "$out/lib/ollama/llama-server"
            echo 'exec "'"$real"'" "$@" --spec-type draft-mtp' >> "$out/lib/ollama/llama-server"
            chmod +x "$out/lib/ollama/llama-server"
          '';
      });
    })
  ];

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
      # NOTE: --spec-type draft-mtp (injected by the ollama-cuda overlay above)
      # is global, so EVERY model loaded here must carry an MTP/nextn head.
      # Models without it fail to load with "context type MTP requested but
      # model doesn't contain MTP layers". The Ollama-library qwen3.6:27b lacks
      # the head, so we pull unsloth's MTP GGUF instead; gemma4:31b has no MTP
      # build and was dropped for this.
      loadModels = ["hf.co/unsloth/Qwen3.6-27B-MTP-GGUF:Q5_K_M"];
      package = pkgs.ollama-cuda;
      environmentVariables = {
        CUDA_VISIBLE_DEVICES = "0";
        OLLAMA_CONTEXT_LENGTH = "81920";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";
        # Qwen3.6 multi-token prediction (the in-model nextn head as the
        # speculative drafter) is enabled via --spec-type draft-mtp, injected
        # into the llama-server argv by the ollama-cuda overlay above. Potential
        # ~1.5-2x decode speedup; see the loadModels NOTE for the
        # all-models-must-have-MTP caveat.
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
