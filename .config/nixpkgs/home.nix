{ config, pkgs, ... }:
{
  home.packages = import ./packages.nix { inherit pkgs; };

  programs = {
    zsh = {
      enable = true;
    };

    emacs = {
      enable = true;
    };
  };
}
