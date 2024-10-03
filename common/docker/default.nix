{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    docker-compose
  ];
  virtualisation = {
    docker = {
      enable = true;
      rootless = {
        enable = true;
        # FIXME(ereslibre): this should be done automatically by
        # NixOS. (https://github.com/NixOS/nixpkgs/pull/344174)
        daemon.settings.features.cdi = true;
        setSocketVariable = true;
      };
    };
  };
  users.users.ereslibre.extraGroups = ["docker"];
}
