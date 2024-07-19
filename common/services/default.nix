{lib, ...}: {
  services = {
    fwupd.enable = true;
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = lib.mkForce "no";
      };
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
  };
}
