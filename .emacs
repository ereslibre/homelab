;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

(add-to-list 'exec-path "/usr/bin/vendor_perl")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar my-packages
  '(monokai-theme projectile helm helm-lsp helm-projectile helm-company yaml-mode magit google-translate diff-hl undo-tree browse-kill-ring ack go-mode markdown-mode haskell-mode rust-mode json-mode yafolding rainbow-delimiters lsp-mode vue-mode neotree company github-review ripgrep powerline yasnippet notmuch git-link protobuf-mode)
  "Ensure this packages are installed")

(require 'cl)
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Magit
(require 'magit)

;; General shortcuts
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "C-c T") '(lambda() (interactive) (term "/sbin/tmux")))

;; Terminal tweaks
(add-hook 'term-mode-hook '(lambda () (setq-local global-hl-line-mode nil)))

;; yasnippet
(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (yas-global-mode 1))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; doc-view
(with-eval-after-load 'doc-view
  (setq doc-view-resolution 300))

;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Git-link
(global-set-key (kbd "C-c g l") 'git-link)
(with-eval-after-load 'git-link
  (setq git-link-use-commit t))

;; Org mode
(require 'org)
(require 'org-notmuch)
(with-eval-after-load 'org
  (setq org-agenda-files '("~/projects/org/inbox.org"
                           "~/projects/org/projects.org"
                           "~/projects/org/tickler.org"
                           "~/projects/org/notes.org"
                           "~/projects/org/someday.org"
                           "~/projects/org/journal.org"
                           "~/projects/org/habits.org"))
  (setq org-agenda-text-search-extra-files
        (append (directory-files-recursively "~/projects/org" "\.org$")
                (directory-files-recursively "~/projects/org" "\.org_archive$")))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c C-o") 'org-open-at-point)
  (setq org-log-done t)
  (setq org-log-repeat 'note)
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-span 8)
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '(("~/projects/org/projects.org" :maxlevel . 3)
                             ("~/projects/org/someday.org" :level . 1)
                             ("~/projects/org/tickler.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-capture-templates
        '(
          ("t" "TODO"
           entry (file "~/projects/org/inbox.org")
           "* TODO %i%?\n  %U"
           :empty-lines 0)
          ("T" "Tickler"
           entry (file "~/projects/org/tickler.org")
           "* %i%?\n  %U"
           :empty-lines 0)
          ("n" "Note"
           entry (file "~/projects/org/notes.org")
           "* %?"
           :empty-lines 0)
          ("j" "Journal Entry"
           entry (file+datetree "~/projects/org/journal.org")
           "* %?"
           :empty-lines 0)
          ))
  (setq org-agenda-custom-commands
        '(("A" "All"
           ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 1)))
            (tags-todo "personal" ((org-agenda-files '("~/projects/org/projects.org"))))
            (tags-todo "work" ((org-agenda-files '("~/projects/org/projects.org"))))
            (tags-todo "hacking" ((org-agenda-files '("~/projects/org/projects.org"))))))
          ("o" "Office"
           ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 1)))
            (tags-todo "work" ((org-agenda-files '("~/projects/org/projects.org")))))
           ((org-agenda-tag-filter-preset '("+work"))))
          ("p" "Personal"
           ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 1)))
            (tags-todo "personal" ((org-agenda-files '("~/projects/org/projects.org")))))
           ((org-agenda-tag-filter-preset '("+personal"))))
          ("h" "Hacking"
           ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 1)))
            (tags-todo "hacking" ((org-agenda-files '("~/projects/org/projects.org")))))
           ((org-agenda-tag-filter-preset '("+hacking"))))
          ("w" "Weekly review"
           ((agenda "" ((org-agenda-start-day nil)
                        (org-agenda-span 'week)))
            (tags-todo "work" ((org-agenda-files '("~/projects/org/projects.org"))))
            (tags-todo "personal" ((org-agenda-files '("~/projects/org/projects.org"))))
            (tags-todo "hacking" ((org-agenda-files '("~/projects/org/projects.org"))))
            (tags "someday" ((org-agenda-files '("~/projects/org/someday.org"))))
            (org-agenda-list-stuck-projects)))))
  (add-to-list 'org-modules 'org-habit))

;; Calendar
(setq calendar-week-start-day 1)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

;; go
(defun go-mode-custom ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-custom)

;; yafolding mode
(require 'yafolding)
(with-eval-after-load 'yafolding
  (add-hook 'prog-mode-hook 'yafolding-mode))

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; lsp mode
(require 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (setq lsp-auto-configure t)
  (setq lsp-diagnostic-package :none)
  (setq lsp-restart 'auto-restart))
(add-hook 'prog-mode-hook 'lsp)

;; Winner mode
(winner-mode 1)

;; Undo tree
(require 'undo-tree)
(with-eval-after-load 'undo-tree
  (global-undo-tree-mode 1))

;; Browse kill ring
(require 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (browse-kill-ring-default-keybindings))

;; powerline
(require 'powerline)
(with-eval-after-load 'powerline
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

;; diff hl
(require 'diff-hl)
(with-eval-after-load 'diff-hl
  (global-diff-hl-mode)
  (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders nil))

;; Misc
(global-unset-key (kbd "C-z"))

;; Default browser
(setq browse-url-browser-function 'browse-url-xdg-open)

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Save last opened files
(setq desktop-save nil)
(desktop-save-mode 0)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Do not break lines
(setq-default truncate-lines t)
(setq-default global-visual-line-mode t)

;; No backups
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Remove whitespaces at the end of line
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Graphical interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Default theme
(load-theme 'monokai t)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Cursor
(blink-cursor-mode 0)

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)

(global-set-key (kbd "C-c H") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-block)

;; Writing style
(setq-default c-basic-indent 2)
(setq-default js-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Syntax highlighting
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default show-paren-style 'parenthesis)

;; Line highlight
(global-hl-line-mode +1)

;; E-mail
(setq mml-secure-openpgp-sign-with-sender t)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(defun ereslibre/fetch-mail-account (account)
  (interactive)
  (let ((name (format "offlineimap-%s" account)))
    (with-current-buffer (get-buffer-create name)
      (special-mode))
    (start-process name name "offlineimap" "-a" account)))

(defun ereslibre/fetch-mail ()
  (interactive)
  (ereslibre/fetch-mail-account "suse")
  (ereslibre/fetch-mail-account "gmail")
  (ereslibre/fetch-mail-account "ereslibre"))

(global-set-key (kbd "C-c m m") 'notmuch)
(global-set-key (kbd "C-c m f") 'ereslibre/fetch-mail)
(autoload 'notmuch "notmuch" "Notmuch mail" t)
(with-eval-after-load 'notmuch
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox")
                                 (:name "unread" :query "tag:inbox AND tag:unread")
                                 (:name "later" :query "tag:later")
                                 (:name "personal" :query "tag:inbox AND NOT tag:work AND NOT to:@suse")
                                 (:name "work" :query "tag:inbox AND (tag:work OR to:@suse)")
                                 (:name "personal-later" :query "tag:later AND NOT tag:work AND NOT to:@suse")
                                 (:name "work-later" :query "tag:later AND (tag:work OR to:@suse)")))
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/sbin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-crypto-process-mime t))

;; Helm
(require 'helm)
(with-eval-after-load 'helm
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-split-window-inside-p t)
  (setq helm-find-files-ignore-thing-at-point t) ; Ignore ffap
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)))

;; helm-lsp
(require 'helm-lsp)
(with-eval-after-load 'helm-lsp
  (global-set-key (kbd "C-c w s") 'helm-lsp-workspace-symbol)
  (global-set-key (kbd "C-c w g") 'helm-lsp-global-workspace-symbol))

;; Projectile
(require 'projectile)
(with-eval-after-load 'projectile
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories (append '(".svn" ".git" ".hg" ".repo" ".vagrant" "build") projectile-globally-ignored-directories))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Neotree
(require 'neotree)
(with-eval-after-load 'neotree
  (setq neo-autorefresh t)
  (setq neo-theme 'ascii)
  (defun neo-window--init (window buffer)
    (neo-buffer--with-resizable-window
     (switch-to-buffer buffer)
     (set-window-parameter window 'no-delete-other-windows t)
     (set-window-parameter window 'no-other-window t)
     (set-window-dedicated-p window t))
    window)
  (defun neo-global--attach ()
    (when neo-global--autorefresh-timer
      (cancel-timer neo-global--autorefresh-timer))
    (when neo-autorefresh
      (setq neo-global--autorefresh-timer
            (run-with-idle-timer 0.1 10 'neo-global--do-autorefresh)))
    (setq neo-global--buffer (get-buffer neo-buffer-name))
    (setq neo-global--window (get-buffer-window
                              neo-global--buffer))
    (neo-global--with-buffer
     (neo-buffer--lock-width))
    (run-hook-with-args 'neo-after-create-hook '(window)))
  (defun neotree-project-dir ()
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-show)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (global-set-key (kbd "C-c n") 'neotree-toggle)
  (global-set-key (kbd "C-c t") 'neotree-find))

;; Frame font
(set-frame-font "Ubuntu Mono-15:Regular")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15:Regular"))
(set-face-attribute 'default t :font "Ubuntu Mono-15:Regular")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 150 :family "Ubuntu Mono"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Ubuntu Mono"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Ubuntu Mono"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 150 :family "Ubuntu Mono"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 150 :family "Ubuntu Mono"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Ubuntu Mono"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 150 :family "Ubuntu Mono"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Ubuntu Mono")))))

;; Go envvars
(setenv "GOPATH" "/home/ereslibre/projects/go")
(add-to-list 'exec-path "/home/ereslibre/projects/go/bin")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/projects/org/inbox.org" "~/projects/org/projects.org" "~/projects/org/tickler.org" "~/projects/org/someday.org" "~/projects/org/journal.org" "~/projects/org/habits.org")))
 '(package-selected-packages
   (quote
    (terraform-mode carbon-now-sh nix-mode adoc-mode helm-lsp nord-theme protobuf-mode git-link notmuch yasnippet powerline monokai-theme github-review helm-projectile helm helm-company projectile groovy-mode lsp-mode company yaml-mode yafolding vue-mode undo-tree rust-mode rainbow-delimiters neotree markdown-mode magit json-mode haskell-mode google-translate go-mode diff-hl browse-kill-ring ack ripgrep)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
