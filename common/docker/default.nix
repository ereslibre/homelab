{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    docker-compose
  ];
  virtualisation = {
    docker = {
      enable = true;
      daemon.settings.features.containerd-snapshotter = true;
      rootless = {
        enable = true;
        setSocketVariable = true;
        daemon.settings.features.containerd-snapshotter = true;
      };
    };
  };
  users.users.ereslibre.extraGroups = ["docker"];
}
