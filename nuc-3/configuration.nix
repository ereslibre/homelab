{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/cloudflared
    ../common/home-node
    ../common/nix-github
    ../common/nixos
    ../common/node
    ../common/openwebui
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/remote-builds
    ../common/services
    ../common/synapse
    ../common/teslamate
    ../common/users
    ../common/vendor/intel
    ../common/vscode-server
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  services.caddy = {
    enable = true;
    virtualHosts = {
      "grafana.teslamate.ereslibre.net".extraConfig = ''
        tls internal
        reverse_proxy http://teslamate:3000
      '';
      "teslamate.ereslibre.net".extraConfig = ''
        tls internal
        reverse_proxy http://teslamate:4000
      '';
    };
  };

  networking = {
    hostName = "nuc-3";
    nat = {
      # Enable NAT on containers so that they have external
      # connectivity.
      enable = true;
      internalInterfaces = ["ve-+"];
      externalInterface = "enp2s0";
    };
  };

  systemd.services.iptables-masquerade = {
    # Provide connectivity on the containers; this masquerade rule is
    # not added automatically.
    description = "Add iptables NAT MASQUERADE rule for container NAT";
    wants = ["network-online.target"];
    after = ["network-online.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        /run/current-system/sw/bin/iptables -t nat -A POSTROUTING -o enp2s0 -j MASQUERADE
      '';
    };
    wantedBy = ["multi-user.target"];
  };

  users.users.ereslibre.extraGroups = ["video"]; # surpillance experiments

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
