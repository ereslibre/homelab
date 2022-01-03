{ config, pkgs, ... }:
{
  home.packages = import ./packages.nix { inherit pkgs; };

  programs = {
    direnv = {
      enable = true;
    };

    emacs = {
      enable = true;
    };

    fzf = {
      enable = true;
    };

    keychain = {
      enable = true;
    };

    zsh = {
      enable = true;
      envExtra = ''
        EDITOR="${pkgs.emacs}/bin/emacsclient --socket-name=main -t"
        GOPATH=$(${pkgs.go}/bin/go env GOPATH)
        GOROOT=$(${pkgs.go}/bin/go env GOROOT)
        GO111MODULE=on
        LANG='en_US.UTF-8'
        PATH=''${HOME}/.bin:''${HOME}/.cargo/bin:''${HOME}/go/bin:/usr/local/bin:''${PATH}
      '';
      initExtra = ''
        token() {
          ${pkgs.yubikey-manager}/bin/ykman oath accounts code | grep -i "$1"
        }
      '';
      oh-my-zsh = {
        enable = true;
        theme = "ereslibre-af-magic";
        custom = "$HOME/dotfiles/.oh-my-zsh";
        plugins = ["git"];
      };
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
  };
}
