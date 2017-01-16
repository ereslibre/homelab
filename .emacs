;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(require 'cl)

;; General shortcuts
(global-set-key "\M-i" 'imenu)
(global-set-key "\C-ct" '(lambda() (interactive) (ansi-term "/bin/zsh")))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-log-repeat 'note)
(setq org-agenda-start-day "-1d")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files '("~/Dropbox/org"))

;; Calendar
(setq calendar-week-start-day 1)

;; Highlight current line
(global-hl-line-mode 1)

(defvar my-packages
  '(projectile projectile-rails helm helm-projectile haml-mode linum-relative linum-off monokai-theme powerline yaml-mode yasnippet magit gist twittering-mode google-translate diff-hl dockerfile-mode undo-tree browse-kill-ring ack hide-comnt go-mode markdown-mode haskell-mode slime rust-mode olivetti terraform-mode)
  "Ensure this packages are installed")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Enable Olivetti
(setq olivetti-body-width 100)
(add-hook 'dired-after-readin-hook #'turn-on-olivetti-mode)
(add-hook 'find-file-hook #'turn-on-olivetti-mode)
(add-hook 'after-init-hook #'turn-on-olivetti-mode)

;; Hide comments
(require 'hide-comnt)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Browse kill ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; diff hl
(global-diff-hl-mode)
(run-with-idle-timer 1 t 'diff-hl-update)
(setq diff-hl-side 'right)
(setq diff-hl-draw-borders nil)

;; Random stuff
(setq magit-last-seen-setup-instructions "1.4.0")

;; Save last opened files
(setq desktop-save t)
(setq desktop-restore-frames nil)
(desktop-save-mode t)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Do not break lines
(set-default 'truncate-lines t)

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

;; Load relative numbers
;; (require 'linum-relative)
;; (require 'linum-off)

;; Cursor
(blink-cursor-mode 0)
(add-hook 'after-change-major-mode-hook
          '(lambda () (hl-line-mode (if (equal major-mode 'term-mode) 0 1))))

;; Enable linum-mode
;; (global-linum-mode 1)
;; (setq-default linum-relative-format "%4s")
;; (setq-default linum-relative-current-symbol "")
;; (with-eval-after-load 'linum
;;   (linum-relative-toggle))

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)
(setq ruby-insert-encoding-magic-comment nil)
(add-hook 'ruby-mode-hook
          (lambda () (hs-minor-mode)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c H") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-block)

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
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (indent-region yas/snippet-beg
                            yas/snippet-end)))
(set-variable 'yas/wrap-around-region nil)
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Twittering mode
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)

;; Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-split-window-in-side-p t)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-directories (append '(".svn" ".git" ".repo" ".vagrant" "assets") projectile-globally-ignored-directories))
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Default font
(set-default-font "mononoki-10:Regular")
(add-to-list 'default-frame-alist '(font . "mononoki-10:Regular"))
(set-face-attribute 'default t :font "mononoki-10:Regular")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1 :family "mononoki"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1 :family "mononoki"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1 :family "mononoki"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1 :family "mononoki"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 1 :family "mononoki"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1 :family "mononoki"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 1 :family "mononoki"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1 :family "mononoki")))))

;; Powerline
(setq ns-use-srgb-colorspace nil)
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'arrow-fade)
(setq powerline-display-buffer-size nil)

;; org-habit
(add-to-list 'org-modules 'org-habit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (terraform-mode olivetti rust-mode slime haskell-mode markdown-mode go-mode hide-comnt ack browse-kill-ring undo-tree dockerfile-mode diff-hl google-translate twittering-mode gist magit yasnippet yaml-mode powerline monokai-theme linum-off linum-relative haml-mode helm-projectile helm projectile-rails projectile))))
