{
  # Needed by the Nvidia container wrapper.
  hardware.graphics.enable32Bit = true;

  virtualisation = {
    docker = {
      enable = true;
      # Set this up to debug issues witht the Docker runtime wrappers
      # for GPU support. If you don't know what this is about, you
      # should probably use `hardware.nvidia-container-toolkit.enable
      # = true;` instead.
      enableNvidia = true;
      rootless = {
        enable = true;
        # FIXME(ereslibre): this should be done automatically by
        # NixOS.
        daemon.settings.features.cdi = true;
      };
    };
  };
  users.users.ereslibre.extraGroups = ["docker"];
}
