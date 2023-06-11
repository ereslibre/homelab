{
  devenv,
  config,
  username,
  pkgs,
  profile,
  ...
}: {
  home = {
    file = import ./dotfiles.nix {inherit username pkgs profile;};
    packages = import ./packages.nix {inherit devenv pkgs;};
  };

  programs = {
    bash.enable = true;
    bat.enable = true;
    direnv.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacs-nox;
    };
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      enableZshIntegration = true;
    };
    git = {
      enable = true;
      delta.enable = true;
      aliases = {
        ci = "commit";
        st = "status";
        co = "checkout";
        br = "branch";
      };
      userName = "Rafael Fernández López";
      extraConfig = {
        color = {
          ui = "auto";
        };
        commit = {
          gpgsign = true;
        };
        core = {
          excludesfile = "~/.gitignore";
        };
        github = {
          user = "ereslibre";
        };
        init = {
          defaultBranch = "main";
        };
        pull = {
          ff = "only";
          rebase = true;
        };
        push = {
          default = "matching";
        };
      };
    };
    htop = {
      enable = true;
      settings.color_scheme = 6;
    };
    jq.enable = true;
    keychain = {
      enable = true;
      enableZshIntegration = true;
      extraFlags = ["--ignore-missing" "--quiet"];
      keys = ["id_ed25519" "id_rsa"];
    };
    less.enable = true;
    pandoc.enable = true;
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {kubernetes = {disabled = false;};};
    };
    tmux = {
      enable = true;
      aggressiveResize = true;
      clock24 = true;
      keyMode = "emacs";
      shortcut = "z";
      terminal = "xterm-direct";
      plugins = with pkgs.tmuxPlugins; [
        sensible
        yank
        {
          plugin = dracula;
          extraConfig = ''
            set -g @dracula-show-powerline true
            set -g @dracula-cpu-display-load true
            set -g @dracula-show-left-icon λ
            set -g @dracula-left-icon-padding 0
            set -g @dracula-show-flags true
            set -g @dracula-refresh-rate 60
            set -g @dracula-plugins "time"
            set -g @dracula-show-timezone false
            set -g @dracula-military-time true
            set -g @dracula-time-format "%a %m/%d %H:%M"
          '';
        }
      ];
      extraConfig = ''
        set -g mouse on

        bind Space copy-mode
        bind C-Space copy-mode
        bind v split-window -h
        bind C-v split-window -h
        bind s split-window -v
        bind C-s split-window -v
        bind-key b choose-tree
        bind-key q kill-window
        bind-key C-q kill-window
        bind-key x kill-pane
        bind-key C-x kill-pane
        bind-key z resize-pane -Z
      '';
    };

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = let
        homeDirectory = config.home.homeDirectory;
      in ''
        export GOPATH="${homeDirectory}/.go"
        export GO111MODULE="on"
        export PATH="${homeDirectory}/.bin:${homeDirectory}/.go/bin:${homeDirectory}/.cargo/bin:''${PATH}"
        export LANG="en_US.UTF-8"
        export LANGUAGE="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"
      '';
      initExtra = ''
        copy_gpg_pubring() {
          scp ~/.gnupg/pubring.kbx "$1":/home/ereslibre/.gnupg/
        }
        fixssh() {
          eval $(tmux show-env -s |grep '^SSH_')
        }
        key_token() {
          ${pkgs.yubikey-manager}/bin/ykman --device "$1" oath accounts code | grep -i "$2"
        }
        nixity-shell() {
          shell_args=()
          for arg in "$@"; do
            shell_args+=("github:ereslibre/nixities#$arg")
          done
          nix shell "''${shell_args[@]}"
        }
        nixity-develop() {
          nix develop github:ereslibre/nixities#$1
        }
        nixity-run() {
          nix run github:ereslibre/nixities#$1 ''${@:2}
        }
        sri() {
          local algo="''${2:-sha256}"
          nix hash to-sri "$algo":$(nix-prefetch-url --type "$algo" "$1")
        }
        token() {
          key_token "$(${pkgs.yubikey-manager}/bin/ykman list --serials | head -n1)" "$1"
        }
      '';
      initExtraFirst = ''
        # This avoids MacOS from destroying Nix on every OS update.
        #   https://github.com/NixOS/nix/issues/3616
        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi
      '';
      oh-my-zsh.enable = true;
      shellAliases = {
        diff = "${pkgs.diffutils}/bin/diff -u --color=auto";
        dive = "dive --source=podman";
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
    enable = pkgs.stdenv.isLinux;
    socketActivation.enable = pkgs.stdenv.isLinux;
    defaultEditor = pkgs.stdenv.isLinux;
  };
}
