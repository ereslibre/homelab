{lib, ...}: {
  services = {
    fwupd.enable = true;
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = lib.mkForce "no";
        # Reap a connection whose client went away (a laptop that
        # slept) in about three minutes rather than TCP keepalive's
        # two hours, so it does not hold forwarded sockets meanwhile.
        ClientAliveInterval = 30;
        ClientAliveCountMax = 6;
      };
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
  };
}
