{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.bat
  ];

  programs = {
    zsh = {
      enable = true;
    };

    emacs = {
      enable = true;
    };
  };
}
