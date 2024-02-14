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

;; font
(set-frame-font "Iosevka-13:Regular")
(add-to-list 'default-frame-alist '(font . "Iosevka-13:Regular"))
(set-face-attribute 'default t :font "Iosevka-13:Regular")

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
