{
  lib,
  pkgs,
  ...
}: {
  # This service creates the GPG socket dir (`/run/user/<id>/gnupg`) automatically.
  systemd.user.services = {
    "gpg-forward-agent-path" = {
      Unit.Description = "Create GnuPG socket directory";
      Service = {
        ExecStart = "${lib.getExe' pkgs.gnupg "gpgconf"} --create-socketdir";
        ExecStop = "";
      };
      Install.WantedBy = ["default.target"];
    };
  };
}
