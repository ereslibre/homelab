{pkgs, ...}: {
  environment = {
    systemPackages = with pkgs; [
      conmon
      podman-compose
    ];
  };
  security.polkit.enable = true;
  users.users.ereslibre.extraGroups = ["podman"];
  virtualisation = {
    containers.enable = true;
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };
}
