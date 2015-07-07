(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; Check that all required packages are installed

(require 'cl)

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
 
(defvar my-packages
  '(projectile helm-projectile projectile-rails haml-mode linum-relative monokai-theme powerline yaml-mode yasnippet magit gist twittering-mode)
  "A list of packages to ensure are installed at launch.")
 
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Random stuff
(setq magit-last-seen-setup-instructions "1.4.0")

;; Save last opened files
(setq desktop-save t)
(desktop-save-mode t)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Do not break lines
(set-default 'truncate-lines t)

;; No backups
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Graphical interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Load relative numbers
(require 'linum-relative)
(setq linum-disabled-modes-list '(twittering-mode org-mode)) (defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

;; Cursor
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(set-face-background 'hl-line "grey22")

;; Enable linum-mode
(global-linum-mode 1)
(setq-default linum-relative-format "%4s \u2502 ")
(setq-default linum-relative-current-symbol "")

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 0)

;; Writing style
(setq-default c-basic-indent 2)
(setq-default js-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 100)

;; Syntax highlighting
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default show-paren-style 'parenthesis)
(require 'haml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(?:php\\)\\'" . ruby-mode))

;; Magit
(require 'magit)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Twittering mode
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)

;; Projectile
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-directories (append '(".svn" ".git" ".repo" ".vagrant") projectile-globally-ignored-directories))
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Default theme
(load-theme 'monokai t)

;; Default font
(set-default-font "Ubuntu Mono 16")

(custom-set-faces
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.3 :family "Ubuntu Mono"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.2 :family "Ubuntu Mono"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1.15 :family "Ubuntu Mono"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1.1 :family "Ubuntu Mono"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :family "Ubuntu Mono"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :family "Ubuntu Mono"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :family "Ubuntu Mono"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :family "Ubuntu Mono")))))
