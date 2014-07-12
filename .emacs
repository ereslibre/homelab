(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/")

;; Special modes
(load "haskell-mode-autoloads.el")

;; Save last opened files
(desktop-save-mode t)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; No backups
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Graphical interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fringe setup
;; (require 'fringe-center-code)

;; Powerline
(require 'powerline)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Load Monokai theme
(require 'monokai-theme)

;; Load relative numbers
(require 'linum-relative)

;; Cursor
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(set-face-background 'hl-line "grey22")

;; Enable linum-mode
(global-linum-mode t)
(setq-default linum-relative-current-symbol "")

;; Writing helpers
(electric-pair-mode 1)

;; Writing style
(setq-default c-basic-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

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

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
