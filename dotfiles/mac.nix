{
  lib,
  pkgs,
  ...
}: {
  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${lib.getExe' pkgs.pinentry_mac "pinentry-mac"}
  '';
}
