;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

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
(global-set-key "\C-ct" '(lambda() (interactive) (ansi-term "/usr/bin/fish")))

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
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-log-repeat 'note)
(setq org-agenda-start-day "-1d")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-capture-templates
      '(
        ("j" "Journal Entry"
         entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?"
         :empty-lines 0)
        ))

;; Calendar
(setq calendar-week-start-day 1)

(defvar my-packages
  '(darkokai-theme projectile helm helm-projectile yaml-mode magit gist google-translate diff-hl undo-tree browse-kill-ring ack hide-comnt go-mode markdown-mode haskell-mode rust-mode json-mode yafolding rainbow-delimiters)
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

;; Hide comments
(require 'hide-comnt)

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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Save last opened files
(setq desktop-save t)
(setq desktop-restore-frames nil)
(desktop-save-mode t)

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

;; Magit
(require 'magit)

;; Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-split-window-in-side-p t)
(setq helm-find-files-ignore-thing-at-point t) ; Ignore ffap
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
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
(setq projectile-globally-ignored-directories (append '(".svn" ".git" ".hg" ".repo" ".vagrant" "build") projectile-globally-ignored-directories))

;; Default font
(set-default-font "Consolas-11:Regular")
(add-to-list 'default-frame-alist '(font . "Consolas-11:Regular"))
(set-face-attribute 'default t :font "Consolas-11:Regular")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 110 :family "Consolas"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 110 :family "Consolas"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 110 :family "Consolas"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 110 :family "Consolas"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 110 :family "Consolas"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 110 :family "Consolas"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 110 :family "Consolas"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 110 :family "Consolas")))))

;; org-habit
(add-to-list 'org-modules 'org-habit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(delete-selection-mode nil)
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
 '(package-selected-packages
   (quote
    (rainbow-delimiters yafolding json-mode darkokai-theme rust-mode haskell-mode markdown-mode go-mode hide-comnt ack browse-kill-ring undo-tree diff-hl google-translate gist magit yaml-mode helm-projectile helm projectile)))
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
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
