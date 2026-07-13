{
  pkgs,
  lib,
  config,
  ...
}: {
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

  # Rootless dockerd runs under RootlessKit with `--copy-up=/etc`, which snapshots
  # /etc (including the /etc/static symlink) into a private tmpfs when the user
  # service starts. After a `nixos-rebuild switch` + GC, that snapshot points at a
  # garbage-collected `-etc` derivation, so /etc/ssl/certs/ca-certificates.crt
  # dangles and image pulls fail with "x509: certificate signed by unknown
  # authority". Point Go's TLS straight at an immutable cacert store path so it
  # never resolves through the frozen /etc/static chain, and restart the daemon
  # whenever cacert changes so it picks up a fresh copy-up.
  #
  # The rootless dockerd user service is installed globally (WantedBy
  # default.target), so every user that gets a systemd user session tries to
  # start it -- including the `builder` remote-build account, which has no
  # /etc/subuid range and therefore fails with "No subuid ranges found",
  # spamming failed units on each SSH login. Gate the unit to the interactive
  # user so systemd *skips* it (ConditionUser, not a hard failure) elsewhere.
  systemd.user.services.docker = lib.mkIf config.virtualisation.docker.rootless.enable {
    environment.SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    restartTriggers = [pkgs.cacert];
    unitConfig.ConditionUser = lib.mkForce "ereslibre";
  };
}
