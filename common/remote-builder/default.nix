{
  config,
  pkgs,
  ...
}: {
  users.groups.builder = {};
  users.users.builder = {
    isSystemUser = true;
    extraGroups = ["wheel"];
    group = "builder";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPgXdZGKpuMlgyDqjUt38Yb0fdkEqMWhSdWKvzFDJG4M"
    ];
  };

  nix.settings.trusted-users = ["builder"];
}
