{ config, pkgs, ... }: {
  home = {
    file = {
      ".bash_profile".text = ''
        if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
        exec ${pkgs.zsh}/bin/zsh
      '';
      ".emacs.d" = {
        source = ./assets/emacs/emacs.d;
        recursive = true;
      };
      ".gitconfig".source = ./assets/git/gitconfig;
      ".gitconfig.suse".source = ./assets/git/gitconfig-suse;
      ".tmux.conf".source = ./assets/tmux.conf;
    };

    packages = import ./packages.nix { inherit pkgs; };
  };

  programs = {
    direnv = { enable = true; };

    emacs = { enable = true; };

    fzf = { enable = true; };

    keychain = { enable = true; };

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = ''
        export EDITOR="${pkgs.emacs}/bin/emacsclient --socket-name=main -t"
        export LANG="en_US.UTF-8"
      '';
      initExtra = ''
        RPROMPT="$RPROMPT $(kubectx_prompt_info)"

        token() {
          ${pkgs.yubikey-manager}/bin/ykman oath accounts code | grep -i "$1"
        }

        keychain_inherit_if_needed() {
          if [ ! -f ~/.ssh/id_rsa ]; then
            ${pkgs.keychain}/bin/keychain --inherit any
          fi
        }

        keychain_inherit_if_needed
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "kubectx" ];
        theme = "bira";
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
