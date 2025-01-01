{pkgs, ...}: {
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    fira-code
    jetbrains-mono
    hasklig
    iosevka
  ];
}
