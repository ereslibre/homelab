{lib, config, pkgs, ...}: {
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

  environment = {
    etc = {
      "nvidia-container-runtime/config.toml".text = ''
        #accept-nvidia-visible-devices-as-volume-mounts = false
        #accept-nvidia-visible-devices-envvar-when-unprivileged = true
        disable-require = false
        supported-driver-capabilities = "compat32,compute,display,graphics,ngx,utility,video"
        #swarm-resource = "DOCKER_RESOURCE_GPU"

        [nvidia-container-cli]
        #debug = "/var/log/nvidia-container-toolkit.log"
        environment = []
        #ldcache = "/etc/ld.so.cache"
        ldconfig = "@/nix/store/303islqk386z1w2g1ngvxnkl4glfpgrs-glibc-2.40-66-bin/sbin/ldconfig"
        load-kmods = true
        #no-cgroups = false
        path = "${lib.getBin pkgs.libnvidia-container}/bin/nvidia-container-cli"
        root = "/run/opengl-driver-32/lib"
        user = "root:video"

        [nvidia-container-runtime]
        #debug = "/var/log/nvidia-container-runtime.log"
        log-level = "info"
        mode = "auto"
        runtimes = ["docker-runc", "runc", "crun"]

        [nvidia-container-runtime.modes]

        [nvidia-container-runtime.modes.cdi]
        annotation-prefixes = ["cdi.k8s.io/"]
        default-kind = "nvidia.com/gpu"
        spec-dirs = ["/etc/cdi", "/var/run/cdi"]

        [nvidia-container-runtime.modes.csv]
        mount-spec-path = "/etc/nvidia-container-runtime/host-files-for-container.d"

        [nvidia-container-runtime.modes.legacy]
        cuda-compat-mode = "ldconfig"

        [nvidia-container-runtime-hook]
        path = "${lib.getOutput "tools" pkgs.nvidia-container-toolkit}/bin/nvidia-container-runtime-hook"
        skip-mode-detection = false

        [nvidia-ctk]
        path = "${lib.getBin pkgs.nvidia-container-toolkit}/bin/nvidia-ctk"
      '';
    };
    systemPackages = with pkgs; [libnvidia-container];
  };
  virtualisation.docker.enableNvidia = true;
  hardware.graphics.enable32Bit = true;
  virtualisation.docker.daemon.settings = {
    default-runtime = "nvidia";
    runtimes.nvidia.path = lib.mkForce "${lib.getOutput "tools" pkgs.nvidia-container-toolkit}/bin/nvidia-container-runtime";
  };

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
