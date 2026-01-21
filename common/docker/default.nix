{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    docker-compose
  ];
  virtualisation = {
    docker = {
      enable = true;
      daemon.settings = {
        dns = ["1.1.1.1"];
        features.containerd-snapshotter = true;
        insecure-registries = ["localhost:3000"];
      };
      rootless = {
        enable = true;
        setSocketVariable = true;
        daemon.settings = {
          dns = ["1.1.1.1"];
          features.containerd-snapshotter = true;
          insecure-registries = ["localhost:3000"];
        };
      };
    };
  };
  users.users.ereslibre.extraGroups = ["docker"];
}
