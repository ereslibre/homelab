{
  lib,
  pkgs,
  ...
}: {
  systemd.user.services = {
    # Create the GPG socket dir (`/run/user/<id>/gnupg`) automatically,
    # and kill any locally-started gpg-agent so it cannot squat on the
    # socket that SSH RemoteForward will bind.
    "gpg-forward-agent-path" = {
      Unit.Description = "Create GnuPG socket directory and kill local agent";
      Service = {
        Type = "oneshot";
        ExecStart = "${lib.getExe' pkgs.gnupg "gpgconf"} --create-socketdir";
        ExecStartPost = "-${lib.getExe' pkgs.gnupg "gpgconf"} --kill gpg-agent";
      };
      Install.WantedBy = ["default.target"];
    };
  };
}
