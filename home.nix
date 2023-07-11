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
    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
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
        copy-gpg-pubring() {
          scp ~/.gnupg/pubring.kbx "$1":/home/ereslibre/.gnupg/
        }
        fixssh() {
          eval $(${pkgs.tmux}/bin/tmux show-env -s |grep '^SSH_')
        }
        hm-upgrade() {
          nix run github:ereslibre/dotfiles#homeConfigurations."''${USER}@''${HOST}".config.activationPackage
        }
        key-token() {
          ${pkgs.yubikey-manager}/bin/ykman --device "$1" oath accounts code | grep -i "$2"
        }
        nixity-shell() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}"
          shell_args=()
          for arg in "$@"; do
            shell_args+=("github:ereslibre/nixities#$arg")
          done
          nix shell $EXTRA_ARGS "''${shell_args[@]}" -c zsh
        }
        nixity-develop() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}"
          nix develop $EXTRA_ARGS github:ereslibre/nixities#$1
        }
        nixity-run() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}"
          nix run $EXTRA_ARGS github:ereslibre/nixities#$1 ''${@:2}
        }
        sri() {
          local algo="''${2:-sha256}"
          nix hash to-sri "$algo":$(nix-prefetch-url --type "$algo" "$1")
        }
        token() {
          key-token "$(${pkgs.yubikey-manager}/bin/ykman list --serials | head -n1)" "$1"
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
        dive = "${pkgs.dive}/bin/dive --source=podman";
        dir = "${pkgs.coreutils}/bin/dir --color=auto";
        grep = "${pkgs.gnugrep}/bin/grep --color=auto";
        k = "${pkgs.kubectl}/bin/kubectl";
        l = "${pkgs.coreutils}/bin/ls --color=auto -CF";
        ll = "${pkgs.coreutils}/bin/ls --color=auto -alF";
        la = "${pkgs.coreutils}/bin/ls --color=auto -A";
        ls = "${pkgs.coreutils}/bin/ls --color=auto";
        vdir = "${pkgs.coreutils}/bin/vdir --color=auto";
      };
    };
  };

  services.emacs = with pkgs.stdenv; {
    enable = isLinux;
    socketActivation.enable = isLinux;
    defaultEditor = isLinux;
  };
}
