{...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/headless-pi
    ../common/nix
    ../common/packages
    ../common/programs
    ../common/services
    ../common/users
  ];

  networking.hostName = "cpi-1";

  # No sops.defaultSopsFile yet — secrets are added once the host's
  # ed25519 SSH host key is created during first install and added to
  # .sops.yaml as the &host-cpi-1 recipient. Same pattern as
  # pi-desktop's first-boot rekey.

  system.stateVersion = "26.05";
}
