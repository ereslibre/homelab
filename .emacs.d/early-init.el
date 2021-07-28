;; general shortcuts
(global-set-key (kbd "M-i") 'helm-imenu)

;; terminal tweaks
(add-hook 'term-mode-hook '(lambda () (setq-local global-hl-line-mode nil)))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; calendar
(setq calendar-week-start-day 1)

;; go
(defun go-mode-custom ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-custom)

;; winner mode
(winner-mode 1)

;; misc
(global-unset-key (kbd "C-z"))

;; default browser
(setq browse-url-browser-function 'browse-url-xdg-open)

;; no welcome screen
(setq-default inhibit-startup-message t)

;; do not break lines
(setq-default truncate-lines t)
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

;; graphical interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; cursor
(blink-cursor-mode 0)

;; writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)

;; writing style
(setq-default c-basic-indent 2)
(setq-default js-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c H") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-block)

;; syntax highlighting
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default show-paren-style 'parenthesis)

;; programming helpers
(add-hook 'prog-mode-hook
          (lambda () (global-set-key (kbd "C-c /") 'comment-or-uncomment-region)))
