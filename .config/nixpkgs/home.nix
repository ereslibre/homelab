{ config, pkgs, ... }:
{
  home.packages = import ./packages.nix { inherit pkgs; };

  programs = {
    zsh = {
      enable = true;
      shellAliases = {
        dir = "dir --color=auto";
        emacs = "${pkgs.emacs}/bin/emacsclient --socket-name=main -t";
        egrep = "egrep --color=auto";
        fgrep = "fgrep --color=auto";
        gpg = "${pkgs.gnupg}/bin/gpg --no-autostart";
        grep = "grep --color=auto";
        k = "${pkgs.kubectl}/bin/kubectl";
        l = "ls --color=auto -CF";
        ll = "ls --color=auto -alF";
        la = "ls --color=auto -A";
        ls = "ls --color=auto";
        vdir = "vdir --color=auto";
      };
    };

    emacs = {
      enable = true;
    };
  };
}
