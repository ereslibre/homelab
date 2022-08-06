{ config, pkgs, ... }: {
  home = {
    file = import ./dotfiles.nix { inherit pkgs; };
    packages = import ./packages.nix { inherit pkgs; };
    stateVersion = "22.05";
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
      settings = { kubernetes = { disabled = false; }; };
    };

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = let homeDirectory = config.home.homeDirectory;
      in ''
        export GOPATH="${homeDirectory}/.go"
        export GO111MODULE="on"
        export PATH="${homeDirectory}/.bin:${homeDirectory}/.go/bin:${homeDirectory}/.cargo/bin:''${PATH}"
        export LANG="en_US.UTF-8"
        export LANGUAGE="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"
      '';
      initExtra = ''
        key_token() {
          ${pkgs.yubikey-manager}/bin/ykman --device "$1" oath accounts code | grep -i "$2"
        }
        token() {
          key_token "$(${pkgs.yubikey-manager}/bin/ykman list --serials | head -n1)" "$1"
        }
        copy_gpg_pubring() {
          scp ~/.gnupg/pubring.kbx "$1":/home/ereslibre/.gnupg/
        }
        devshell() {
          nix develop --impure --expr "with import <nixpkgs> {}; pkgs.mkShell { packages = with pkgs; [ $* ]; }"
        }
      '';
      oh-my-zsh.enable = true;
      shellAliases = {
        dir = "dir --color=auto";
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

  services.emacs = {
    enable = true;
    socketActivation.enable = true;
  };
}
