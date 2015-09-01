(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives
;             '("marmalade" . "https://marmalade-repo.org/packages/") t)

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

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")

(defvar my-packages
  '(projectile projectile-rails helm helm-projectile haml-mode linum-relative monokai-theme powerline yaml-mode yasnippet magit gist twittering-mode google-translate auto-complete diff-hl dockerfile-mode docker)
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

;; Docker integration
(require 'docker)

;; Auto complete
(ac-config-default)

;; diff hl
(global-diff-hl-mode)
(setq diff-hl-draw-borders nil)
(run-with-idle-timer 1 t 'diff-hl-update)

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
(require 'linum-relative)
(setq linum-disabled-modes-list '(twittering-mode term-mode org-mode text-mode doc-view-mode dired-mode dired-x-mode image-mode)) (defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

;; Cursor
(blink-cursor-mode 0)
(add-hook 'after-change-major-mode-hook
          '(lambda () (hl-line-mode (if (equal major-mode 'term-mode) 0 1))))

;; Enable linum-mode
(global-linum-mode 1)
(setq-default linum-relative-format " %4s | ")
(setq-default linum-relative-current-symbol "")

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)
(setq ruby-insert-encoding-magic-comment nil)

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
(setq projectile-globally-ignored-directories (append '(".svn" ".git" ".repo" ".vagrant") projectile-globally-ignored-directories))
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Default font
(set-default-font "Hack-12")

(custom-set-faces
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.3 :family "Hack"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.2 :family "Hack"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1.15 :family "Hack"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1.1 :family "Hack"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :family "Hack"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :family "Hack"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :family "Hack"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :family "Hack")))))

;; Powerline
(setq ns-use-srgb-colorspace nil)
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'wave)
(setq powerline-display-buffer-size nil)
