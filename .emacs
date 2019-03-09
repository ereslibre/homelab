;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(require 'cl)

;; General shortcuts
(global-set-key "\M-i" 'imenu)
(global-set-key "\C-ct" '(lambda() (interactive) (ansi-term "/usr/bin/fish")))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; doc-view
(require 'doc-view)
(setq doc-view-resolution 300)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-log-repeat 'note)
(setq org-agenda-start-day "-1d")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files '("/run/media/ereslibre/ereslibre/org"))
(setq org-capture-templates
      '(
        ("j" "Journal Entry"
         entry (file+datetree "/run/media/ereslibre/ereslibre/org/journal.org")
         "* %?"
         :empty-lines 0)
        ))

;; Calendar
(setq calendar-week-start-day 1)

(defvar my-packages
  '(darkokai-theme projectile helm helm-projectile helm-company yaml-mode magit gist google-translate diff-hl undo-tree browse-kill-ring ack go-mode markdown-mode haskell-mode rust-mode json-mode yafolding rainbow-delimiters lsp-mode vue-mode neotree company company-lsp github-review)
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

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
     (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
     ))

;; go
(defun go-mode-custom ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-custom)

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; lsp mode
(require 'lsp-mode)
(add-hook 'prog-mode-hook 'lsp)
(setq lsp-prefer-flymake :none)
(setq lsp-restart 'ignore)

;; Winner mode
(winner-mode 1)

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
(setq diff-hl-side 'right)
(setq diff-hl-draw-borders nil)

;; Misc
(global-unset-key (kbd "C-z"))
(setq magit-last-seen-setup-instructions "1.4.0")

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Save last opened files
(setq desktop-save nil)
(setq desktop-restore-frames nil)
(desktop-save-mode nil)

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
(load-theme 'darkokai t)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Cursor
(blink-cursor-mode 0)

;; Writing helpers
(electric-pair-mode 1)
(electric-indent-mode 1)

(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

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

;; Magit
(require 'magit)

;; Helm
(require 'helm-config)
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
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; Projectile
(projectile-mode +1)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-directories (append '(".svn" ".git" ".hg" ".repo" ".vagrant" "build") projectile-globally-ignored-directories))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Neotree
(require 'neotree)
(setq neo-autorefresh t)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
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

;; Default font
(set-default-font "Consolas-13:Regular")
(add-to-list 'default-frame-alist '(font . "Consolas-13:Regular"))
(set-face-attribute 'default t :font "Consolas-13:Regular")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 130 :family "Consolas"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 130 :family "Consolas"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 130 :family "Consolas"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 130 :family "Consolas"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 130 :family "Consolas"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 130 :family "Consolas"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 130 :family "Consolas"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 130 :family "Consolas")))))

;; org-habit
(add-to-list 'org-modules 'org-habit)

;; Go envvars
(setenv "GOPATH" "/home/ereslibre/projects/go")
(add-to-list 'exec-path "/home/ereslibre/projects/go/bin")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (github-review helm-projectile company-lsp helm helm-company projectile groovy-mode lsp-mode company yaml-mode yafolding vue-mode undo-tree rust-mode rainbow-delimiters neotree markdown-mode magit json-mode haskell-mode google-translate go-mode gist diff-hl darkokai-theme browse-kill-ring ack))))
