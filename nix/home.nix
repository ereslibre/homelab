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
        export EDITOR="${pkgs.emacs}/bin/emacsclient --socket-name=main -t"
        export GOPATH="$(${pkgs.go}/bin/go env GOPATH)"
        export GOROOT="$(${pkgs.go}/bin/go env GOROOT)"
        export GO111MODULE="on"
        export LANG="en_US.UTF-8"
        export PATH="''${HOME}/.bin:''${HOME}/.cargo/bin:''${HOME}/go/bin:/usr/local/bin:''${PATH}"
      '';
      initExtra = ''
        RPROMPT="$RPROMPT $(kubectx_prompt_info)"
        token() {
          ${pkgs.yubikey-manager}/bin/ykman oath accounts code | grep -i "$1"
        }
      '';
      oh-my-zsh = {
        enable = true;
        theme = "bira";
        plugins = ["git" "kubectx"];
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
