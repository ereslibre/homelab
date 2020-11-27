;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

(add-to-list 'exec-path "/usr/bin/vendor_perl")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.ghcup/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; General shortcuts
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "C-c T") '(lambda() (interactive) (term "/sbin/tmux")))

;; Terminal tweaks
(add-hook 'term-mode-hook '(lambda () (setq-local global-hl-line-mode nil)))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Calendar
(setq calendar-week-start-day 1)

;; go
(defun go-mode-custom ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-custom)

;; Winner mode
(winner-mode 1)

;; Misc
(global-unset-key (kbd "C-z"))

;; Default browser
(setq browse-url-browser-function 'browse-url-xdg-open)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Do not break lines
(setq-default truncate-lines t)
(setq-default global-visual-line-mode t)

;; No backups
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Save last opened files
(setq desktop-save nil)
(desktop-save-mode 0)

;; Line highlight
(global-hl-line-mode +1)

;; Remove whitespaces at the end of line
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Graphical interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Cursor
(blink-cursor-mode 0)

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)

;; Writing style
(setq-default c-basic-indent 2)
(setq-default js-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c H") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-block)

;; Syntax highlighting
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default show-paren-style 'parenthesis)

;; E-mail
(setq mml-secure-openpgp-sign-with-sender t)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;; Notmuch
(global-set-key (kbd "C-c m m") 'notmuch)
