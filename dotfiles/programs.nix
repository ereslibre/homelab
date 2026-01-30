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
  emacs = {nox}: let
    emacsConfig = (import ./emacs-config.nix {inherit mainlyRemote nox isDarwin;}) {inherit pkgs;};
  in
    emacsConfig.emacsBinary;
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
      "/run/wrappers/bin"
      # This is fundamentally for nix-darwin, given this is not added
      # to the $PATH: https://github.com/LnL7/nix-darwin/issues/922
      "/run/current-system/sw/bin"
    ];
    sessionVariables = {
      EDITOR = emacs {nox = true;};
      TERM = "konsole-direct";
    };
    file = lib.mkIf (!mainlyRemote) {
      ".config/ghostty/config".text = ''
        font-family = "JetBrains Mono"
        font-size = "12"
        theme = Dracula
        cursor-color = white
        cursor-style = block
        cursor-style-blink = false
        shell-integration-features = no-cursor
        copy-on-select = clipboard
        window-padding-x = 4
        window-padding-y = 4
      '';
    };
  };
  programs = {
    bash = {
      enable = true;
      inherit (shellExtras) profileExtra shellAliases;
    };
    bat.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    emacs = let
      emacsConfig = (import ./emacs-config.nix {
        inherit mainlyRemote isDarwin;
        nox = false;
      }) {inherit pkgs;};
    in {
      enable = true;
      package = emacsConfig.customEmacs;
      extraConfig = ''
        ;; Configure tree-sitter grammar path
        (setq treesit-extra-load-path '("${emacsConfig.treesit-grammars}/lib"))

        ;; Emacs configuration with Nix-managed packages
        ;; All packages are provided by Nix, so no package.el or use-package :ensure needed

        ;; Load use-package for configuration only (packages already installed via Nix)
        (require 'use-package)

        ;;; Basic UI and Editor Settings

        ;; word wrap
        (add-hook 'text-mode-hook 'turn-on-auto-fill)

        ;; calendar
        (setq calendar-week-start-day 1)

        ;; misc -- tmux prefix
        (global-unset-key (kbd "C-z"))

        ;; no menubar, scrollbar or toolbar
        (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
        (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
        (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

        ;; no welcome screen
        (setq-default inhibit-startup-message t)

        ;; do not break lines
        (setq-default truncate-lines nil)
        (setq-default global-visual-line-mode t)

        ;; no backups
        (setq-default make-backup-files nil)
        (setq-default auto-save-default nil)

        ;; save last opened files
        (setq desktop-save nil)
        (desktop-save-mode 0)

        ;; line highlight
        (global-hl-line-mode +1)

        ;; remove whitespaces at the end of line
        (add-hook 'before-save-hook 'delete-trailing-whitespace)

        ;; lines and columns
        (line-number-mode 1)
        (column-number-mode 1)

        ;; winner mode
        (winner-mode 1)

        ;; cursor
        (blink-cursor-mode 0)

        ;; writing helpers
        (electric-pair-mode 1)
        (electric-indent-mode 1)

        ;; writing style
        (setq-default tab-width 2)
        (setq-default indent-tabs-mode nil)

        ;; syntax highlighting
        (show-paren-mode 1)
        (setq-default show-paren-delay 0)
        (setq-default show-paren-style 'mixed)

        ;; programming helpers
        (add-hook 'prog-mode-hook
                  (lambda () (global-set-key (kbd "C-c /") 'comment-or-uncomment-region)))
        (add-hook 'prog-mode-hook 'display-line-numbers-mode)

        ;; font
        (set-frame-font "Iosevka-13:Regular")
        (add-to-list 'default-frame-alist '(font . "Iosevka-13:Regular"))
        (set-face-attribute 'default t :font "Iosevka-13:Regular")

        ;;; Package Configuration

        (use-package ace-window
          :bind (("C-x o" . ace-window)
                 ("C-x O" . ace-swap-window)))

        (use-package dumb-jump
          :config
          (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

        (use-package dracula-theme
          :demand t
          :config
          (load-theme 'dracula t))

        (use-package project
          :demand t
          :config
          (setq project-switch-commands 'helm-project))

        (use-package magit
          :demand t
          :defer 3)

        (use-package yasnippet
          :demand t
          :config
          (yas-global-mode 1))

        (use-package git-link
          :bind ("C-c g l" . git-link)
          :init
          (setq git-link-use-commit t))

        (use-package company
          :demand t
          :config
          (add-hook 'after-init-hook 'global-company-mode))

        (use-package gptel
          :demand t
          :config
          (setq
           gptel-model 'qwen2.5-coder:32b
           gptel-backend (gptel-make-ollama "ollama"
                           :host "hulk.ereslibre.net:11434"
                           :stream t
                           :models '(qwen2.5-coder:32b
                                     deepseek-r1:32b))))

        (use-package lsp-mode
          :demand t
          :custom
          (lsp-eldoc-render-all t)
          (lsp-idle-delay 0.6)
          (lsp-inlay-hint-enable t)
          (lsp-rust-analyzer-cargo-watch-command "clippy")
          (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
          (lsp-rust-analyzer-display-chaining-hints t)
          (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
          (lsp-rust-analyzer-display-closure-return-type-hints t)
          (lsp-rust-analyzer-display-parameter-hints nil)
          (lsp-rust-analyzer-display-reborrow-hints nil)
          :init
          (setq lsp-restart 'interactive)
          :hook (
                 (rust-mode . lsp)
                 (rust-ts-mode . lsp)
                 (typescript-ts-mode . lsp)
                 (tsx-ts-mode . lsp)
                 (javascript-ts-mode . lsp)
                 (python-ts-mode . lsp)
                 (go-ts-mode . lsp)
                 (bash-ts-mode . lsp)))

        ;; treesit-auto: Automatically use tree-sitter modes
        ;; Note: Grammar installation is disabled because grammars are provided by Nix
        (use-package treesit-auto
          :demand t
          :custom
          (treesit-auto-install nil) ; Grammars are managed by Nix, don't auto-install
          :config
          (treesit-auto-add-to-auto-mode-alist 'all) ; Use tree-sitter for all supported languages
          (global-treesit-auto-mode))

        (use-package undo-tree
          :demand t
          :config
          (setq undo-tree-auto-save-history nil)
          :custom
          (global-undo-tree-mode 1))

        (use-package browse-kill-ring
          :demand t
          :config
          (browse-kill-ring-default-keybindings))

        (use-package powerline
          :demand t
          :config
          (setq powerline-default-separator 'wave)
          (powerline-default-theme))

        (use-package rainbow-delimiters
          :demand t
          :config
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

        (use-package helm
          :demand t
          :bind (("C-x C-f" . helm-find-files)
                 ("M-x" . helm-M-x)
                 ([remap occur] . helm-occur)
                 ([remap list-buffers] . helm-buffers-list)
                 ([remap dabbrev-expand] . helm-dabbrev))
          :config
          (require 'helm-imenu)
          (require 'helm-project)
          (helm-mode 1)
          (helm-autoresize-mode t)
          (global-set-key (kbd "M-i") 'helm-imenu)
          (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-z")  'helm-select-action))

        (use-package neotree
          :bind (("C-c n" . neotree-toggle)
                 ("C-c t" . neotree-find))
          :config
          (setq neo-autorefresh t)
          (setq neo-theme 'ascii))

        (use-package org
          :demand t
          :bind (("C-c l" . org-store-link)
                 ("C-c a" . org-agenda)
                 ("C-c c" . org-capture)
                 ("C-c C-o" . org-open-at-point)
                 ("C-c ," . org-insert-structure-template))
          :config
          (setq org-directory "~/.org")
          (setq org-agenda-files '("~/.org/habits.org"
        			                     "~/.org/inbox.org"
        			                     "~/.org/journal.org"
        			                     "~/.org/notes.org"
        			                     "~/.org/papers.org"
        			                     "~/.org/projects.org"
        			                     "~/.org/reminders.org"
        			                     "~/.org/someday.org"
        			                     "~/.org/tickler.org"))
          (setq org-agenda-text-search-extra-files
        	      (append (directory-files-recursively "~/.org" "\.org$")
        		            (directory-files-recursively "~/.org" "\.org_archive$")))
          (setq org-log-repeat 'note)
          (setq org-agenda-span 'day)
          (setq org-agenda-start-on-weekday nil)
          (setq org-refile-use-outline-path 'file)
          (setq org-outline-path-complete-in-steps nil)
          (defun update-org-refile-files (ignored)
            (setq org-refile-files (file-expand-wildcards "~/.org/*.org")))
          (update-org-refile-files nil)
          (setq org-refile-targets '((org-refile-files :maxlevel . 3)))
          (setq org-todo-keywords '((sequence "TODO(t)" "DOING(g!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")))
          (setq org-capture-templates
        	      '(("t" "TODO"
        	         entry (file "~/.org/inbox.org")
        	         "* TODO %i%?\n  %U"
        	         :empty-lines 0)
        	        ("T" "Tickler"
        	         entry (file "~/.org/tickler.org")
        	         "* %i%?\n  %U"
        	         :empty-lines 0)
        	        ("n" "Note"
        	         entry (file "~/.org/notes.org")
        	         "* %?"
        	         :empty-lines 0)
        	        ("j" "Journal Entry"
        	         entry (file+datetree "~/.org/journal.org")
        	         "* %<%H:%M> %?"
        	         :empty-lines 0)
        	        ))
          (setq org-agenda-custom-commands
        	      '(("A" "All"
        	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
         	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org"))))
        	          (tags-todo "+work" ((org-agenda-files '("~/.org/projects.org"))))
        	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org"))))))
        	        ("o" "Office"
        	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
        	          (tags-todo "+work" ((org-agenda-files '("~/.org"))))))
        	        ("p" "Personal"
        	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
        	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org")))))
        	         ((org-agenda-tag-filter-preset '("+personal"))))
        	        ("h" "Hacking"
        	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
        	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org")))))
        	         ((org-agenda-tag-filter-preset '("+hacking"))))
        	        ("l" "Logbook"
        	         ((tags "+logbook" ((org-agenda-files '("~/.org")))))
        	         ((org-agenda-tag-filter-preset '("+work"))))
        	        ("w" "Weekly review"
        	         ((agenda "" ((org-agenda-start-day nil)
        			                  (org-agenda-span 'week)))
        	          (tags-todo "+work" ((org-agenda-files '("~/.org/projects.org"))))
        	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org"))))
        	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org"))))
        	          (tags "+someday" ((org-agenda-files '("~/.org/someday.org"))))))))
          (require 'org-journal)
          (require 'org-agenda)
          (add-to-list 'org-modules 'org-habit)
          (add-hook 'before-save-hook (lambda ()
                                        (when (eq major-mode 'org-mode) (org-align-tags t))))
          (require 'ob-shell)
          (org-babel-do-load-languages
           'org-babel-load-languages
           '((shell . t))))

        (use-package sublimity
          :demand t
          :custom
          (sublimity-mode 1)
          :config
          (require 'sublimity-attractive)
          ;; When on org-agenda-mode, tags are aligned first, then sublimity
          ;; resizes and centers, and tags are left out of screen. Redo the
          ;; agenda.
          (add-hook 'sublimity--window-change-functions
                    (lambda ()
                      (when (eq major-mode 'org-agenda-mode) (org-agenda-redo)))))

        ;; Load custom file if it exists
        (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
        (when (file-exists-p custom-file)
          (load custom-file))
      '';
    };
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      enableZshIntegration = true;
    };
    delta = {
      enable = true;
      enableGitIntegration = true;
    };
    git = {
      enable = true;
      settings = {
        alias = {
          ci = "commit";
          st = "status";
          co = "checkout";
          br = "branch";
        };
        user = {
          name = "Rafael Fernández López";
          email =
            if profile == "personal"
            then "ereslibre@ereslibre.es"
            else "ereslibre@curried.software";
        };
        color.ui = "auto";
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
    ssh = let
      mapWithDomain = host: {
        "${host}" = {
          hostname = "${host}.ereslibre.net";
        };
      };
    in {
      enable = true;
      enableDefaultConfig = false;
      extraConfig = ''
        Include config.d/*
      '';
      matchBlocks =
        {
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
          "ubuntu-1.hulk" = {
            extraOptions = {
              "HostName" = "192.168.122.200";
              "ProxyJump" = "hulk.ereslibre.net";
            };
          };
          "*" = {
            compression = true;
            forwardAgent = true;
            forwardX11 = false;
            serverAliveCountMax = 10;
            serverAliveInterval = 20;
          };
        }
        // (mapWithDomain "hulk")
        // (mapWithDomain "nuc-1")
        // (mapWithDomain "nuc-2")
        // (mapWithDomain "nuc-3");
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
            # Enable clipboard through tmux in remote session
            set -as terminal-features ',konsole-direct:clipboard'

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
      extraConfig =
        (
          ''
            set -g default-terminal "konsole-direct"
          ''
        )
        + ''
          set -gu default-command
          set -g default-shell "$SHELL"
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
      envExtra = ''
        export GIT_EDITOR="${emacs {nox = true;}}"
        export GOPATH="${config.home.homeDirectory}/.go"
        export PATH="${config.home.homeDirectory}/.bin:${config.home.homeDirectory}/.global-npm/bin:${config.home.homeDirectory}/.go/bin:${config.home.homeDirectory}/.cargo/bin:''${PATH}"
        export LANG="en_US.UTF-8"
        export LANGUAGE="en_US.UTF-8"
        export LC_ALL="en_US.UTF-8"
      '';
      initContent = lib.mkBefore ''
        # This avoids MacOS from destroying Nix on every OS update.
        #   https://github.com/NixOS/nix/issues/3616
        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi

        copy-gpg-pubring() {
          scp ~/.gnupg/pubring.kbx "$1":/home/${username}/.gnupg/
        }
        refresh-gpg-card() {
          gpg-connect-agent "scd serialno" "learn --force" /bye
        }
        superclear() {
          # clear and empty scrollback
          printf "\033[H\033[2J\033[3J"
        }
        fixssh() {
          eval $(${pkgs.tmux}/bin/tmux show-env -s | grep '^SSH_')
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
