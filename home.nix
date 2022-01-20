{ config, pkgs, ... }: {
  home = {
    file = import ./dotfiles.nix { inherit config pkgs; };
    packages = import ./packages.nix { inherit pkgs; };
  };

  programs = {
    bash.enable = true;
    direnv.enable = true;
    emacs.enable = true;
    fzf.enable = true;
    keychain.enable = true;

    starship = {
      enable = true;
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = let homeDirectory = config.home.homeDirectory;
      in ''
        export PATH="${homeDirectory}/.bin:${homeDirectory}/.cargo/bin:''${PATH}"
        export EDITOR="${pkgs.emacs}/bin/emacsclient -t"
        export LANG="en_US.UTF-8"
        export LANGUAGE="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"
      '';
      initExtra = ''
        token() {
          ${pkgs.yubikey-manager}/bin/ykman oath accounts code | grep -i "$1"
        }
      '';
      shellAliases = {
        dir = "dir --color=auto";
        emacs = "${pkgs.emacs}/bin/emacsclient -t";
        egrep = "egrep --color=auto";
        fgrep = "fgrep --color=auto";
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
