{
  username,
  profile,
  mainlyRemote,
  isDarwin,
  isLinux,
}: {
  config,
  lib,
  pkgs,
  ...
}: let
  maybeWrappedEmacsClient = emacs:
    if isDarwin
    then
      (pkgs.writeShellScriptBin "emacsclient" ''
        exec env XDG_RUNTIME_DIR="$HOME/.emacs.d" ${emacs}/bin/emacsclient "$@"
      '')
    else emacs;
  emacsBinary = {nox}:
    if mainlyRemote || nox
    then "${maybeWrappedEmacsClient pkgs.emacs-nox}/bin/emacsclient --tty"
    else
      (let
        script = pkgs.writeShellScriptBin "emacsclient" ''
          exec ${maybeWrappedEmacsClient pkgs.emacs}/bin/emacsclient --create-frame --no-wait -e "(progn (select-frame-set-input-focus (selected-frame)) (toggle-frame-maximized) (find-file (expand-file-name \"$1\")))"
        '';
      in "${script}/bin/emacsclient");
  emacs = {nox}: emacsBinary {inherit nox;};
  k = "${pkgs.kubectl}/bin/kubectl";
  shellExtras = {
    profileExtra = ''
      if [ -e ''${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/nix.sh; fi
      if [ -e ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . ''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
    '';
    shellAliases = {
      emacs = emacs {nox = false;};
      emacs-nox = emacs {nox = true;};
    };
  };
in {
  home = {
    sessionPath = [
      # This is fundamentally for nix-darwin, given this is not added
      # to the $PATH: https://github.com/LnL7/nix-darwin/issues/922
      "/run/current-system/sw/bin"
    ];
    sessionVariables = {
      EDITOR = emacs {nox = true;};
    };
  };
  programs = {
    alacritty = {
      enable = true;
      settings = {
        "import" = [
          ./assets/alacritty/dracula.toml
        ];
        font = {
          normal = {
            family = "Fira Code";
            style = "Regular";
          };
          bold = {
            family = "Fira Code";
            style = "Bold";
          };
          italic = {
            family = "Fira Code";
            style = "Italic";
          };
          bold_italic = {
            family = "Fira Code";
            style = "Bold Italic";
          };
          size = 12;
        };
        cursor = {
          style = {
            shape = "Block";
            blinking = "Never";
          };
        };
        window = {
          decorations = "full";
          decorations_theme_variant = "Dark";
          dynamic_padding = true;
          opacity = 0.9;
          option_as_alt = "Both";
        };
      };
    };
    bash = {
      enable = true;
      inherit (shellExtras) profileExtra shellAliases;
    };
    bat.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    emacs = {
      enable = true;
      package =
        if mainlyRemote
        then pkgs.emacs-nox
        else pkgs.emacs;
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
      userEmail =
        if profile == "personal"
        then "ereslibre@ereslibre.es"
        else "ereslibre@curried.software";
      extraConfig = {
        color.ui = "auto";
        commit.gpgsign = true;
        core.excludesfile = "${./assets/git/gitignore}";
        github.user = "ereslibre";
        init.defaultBranch = "main";
        pull = {
          ff = "only";
          rebase = true;
        };
        push.default = "matching";
      };
    };
    htop = {
      enable = true;
      settings.color_scheme = 6;
    };
    jq.enable = true;
    less.enable = true;
    pandoc.enable = true;
    ripgrep.enable = true;
    ssh = {
      enable = true;
      forwardAgent = true;
      matchBlocks = {
        "ereslibre-1.oracle.cloud ereslibre-2.oracle.cloud" = {
          user = "ubuntu";
        };
        "hulk hulk.ereslibre.net nuc-1 nuc-1.ereslibre.net nuc-2 nuc-2.ereslibre.net nuc-3 nuc-3.ereslibre.net" = {
          extraOptions = {
            "RemoteForward" = "/run/user/1000/gnupg/S.gpg-agent /Users/${username}/.gnupg/S.gpg-agent.extra";
          };
        };
        "10.0.1.*".extraOptions = {
          "StrictHostKeyChecking" = "no";
        };
        "10.0.2.*".extraOptions = {
          "StrictHostKeyChecking" = "no";
        };
        "10.0.3.*".extraOptions = {
          "StrictHostKeyChecking" = "no";
        };
        "10.0.4.*".extraOptions = {
          "StrictHostKeyChecking" = "no";
        };
        "192.168.*".extraOptions = {
          "StrictHostKeyChecking" = "no";
        };
        "*" = {
          compression = true;
          forwardX11 = false;
          serverAliveCountMax = 10;
          serverAliveInterval = 20;
        };
      };
    };
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
      terminal = "xterm";
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

    vscode.enable = !mainlyRemote;

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = ''
        export TERM=xterm-direct
        export GIT_EDITOR="${emacs {nox = true;}}"
        export GOPATH="${config.home.homeDirectory}/.go"
        export PATH="${config.home.homeDirectory}/.bin:${config.home.homeDirectory}/.go/bin:${config.home.homeDirectory}/.cargo/bin:''${PATH}"
        export LANG="en_US.UTF-8"
        export LANGUAGE="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"
      '';
      initExtra = ''
        copy-gpg-pubring() {
          scp ~/.gnupg/pubring.kbx "$1":/home/${username}/.gnupg/
        }
        refresh-gpg-card() {
          gpg-connect-agent "scd serialno" "learn --force" /bye
        }
        fixssh() {
          eval $(${pkgs.tmux}/bin/tmux show-env -s |grep '^SSH_')
        }
        key-token() {
          ${pkgs.yubikey-manager}/bin/ykman --device "$1" oath accounts code | grep -i "$2"
        }
        nixity-new() {
          nix flake new -t github:ereslibre/nixities#"$1"
        }
        nixity-shell() {
          shell_args=()
          for arg in "$@"; do
            shell_args+=("github:ereslibre/nixities#$arg")
          done
          nix shell $EXTRA_ARGS "''${shell_args[@]}" -c zsh
        }
        nixity-shell-offline() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}" nixity-shell $1 ''${@:2}
        }
        nixity-develop() {
          nix develop $EXTRA_ARGS github:ereslibre/nixities#$1
        }
        nixity-develop-offline() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}" nixity-develop $1 ''${@:2}
        }
        nixity-run() {
          nix run $EXTRA_ARGS github:ereslibre/nixities#$1 ''${@:2}
        }
        nixity-run-offline() {
          EXTRA_ARGS="''${EXTRA_ARGS:---offline}" nixity-run $1 ''${@:2}
        }
        setup-devbox() {
          ssh "$1" mkdir -p '~/.ssh'
          scp ~/.ssh/id_* "$1":'~/.ssh'
          ssh "$1" chown -R ${username}:users '~/.ssh'
        }
        sri() {
          local algo="''${2:-sha256}"
          nix hash to-sri "$algo":$(nix-prefetch-url --type "$algo" $EXTRA_ARGS "$1")
        }
        token() {
          key-token "$(${pkgs.yubikey-manager}/bin/ykman list --serials | head -n1)" "$1"
        }
        shiori-list() {
          ${k} exec deployment/shiori -n shiori -- shiori print -l "$@"
        }
        shiori-search() {
          ${k} exec deployment/shiori -n shiori -- shiori print -l "''${@:2}" -s "$1"
        }
        ,() {
          nixity-run $1 -- "''${@:2}"
        }
        ,,() {
          nixity-develop "$@"
        }
        ,,,() {
          local targetdir=$(mktemp -d)
          pushd "$targetdir" > /dev/null
          ${pkgs.dhall}/bin/dhall text <<< "${./assets/config.dhall} { derivations = \"$@\" }" > flake.nix
          popd > /dev/null
          nix develop $EXTRA_ARGS "$targetdir"
          rm -rf "$targetdir"
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
      shellAliases =
        {
          inherit k;

          diff = "${pkgs.diffutils}/bin/diff -u --color=auto";
          dive = "${pkgs.dive}/bin/dive --source=podman";
          dir = "${pkgs.coreutils}/bin/dir --color=auto";
          grep = "${pkgs.gnugrep}/bin/grep --color=auto";
          l = "${pkgs.coreutils}/bin/ls --color=auto -CF";
          ll = "${pkgs.coreutils}/bin/ls --color=auto -alF";
          la = "${pkgs.coreutils}/bin/ls --color=auto -A";
          ls = "${pkgs.coreutils}/bin/ls --color=auto";
          # xterm-direct is not in terminfo for MacOS
          reset = "reset xterm";
          vdir = "${pkgs.coreutils}/bin/vdir --color=auto";
        }
        // shellExtras.shellAliases;
      inherit (shellExtras) profileExtra;
    };
  };

  services = {
    emacs.enable = isLinux;
    ssh-agent.enable = isLinux;
  };
}
