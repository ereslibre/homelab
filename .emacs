;; Default shell
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(monokai-theme projectile helm helm-projectile helm-company yaml-mode magit google-translate diff-hl undo-tree browse-kill-ring ack go-mode markdown-mode haskell-mode rust-mode json-mode yafolding rainbow-delimiters lsp-mode vue-mode neotree company company-lsp github-review)
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

;; General shortcuts
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-c T") '(lambda() (interactive) (term "/sbin/tmux")))

;; Terminal tweaks
(add-hook 'term-mode-hook '(lambda () (setq-local global-hl-line-mode nil)))

;; imenu
(set-default
 'imenu-after-jump-hook (recenter (/ (window-height) 2)))
(set-default
 'imenu-auto-rescan t)

;; doc-view
(with-eval-after-load 'doc-view
  (setq doc-view-resolution 300))

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Org mode
(require 'org)
(with-eval-after-load 'org
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-log-done t)
  (setq org-log-repeat 'note)
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-files '("/run/media/ereslibre/ereslibre/org"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-capture-templates
        '(
          ("t" "Thought"
           entry (file "/run/media/ereslibre/ereslibre/org/thoughts.org")
           "* %?"
           :empty-lines 0)
          ("k" "Task"
           entry (file "/run/media/ereslibre/ereslibre/org/tasks.org")
           "* %?"
           :empty-lines 0)
          ("n" "Note"
           entry (file "/run/media/ereslibre/ereslibre/org/notes.org")
           "* %?"
           :empty-lines 0)
          ("j" "Journal Entry"
           entry (file+datetree "/run/media/ereslibre/ereslibre/org/journal.org")
           "* %?"
           :empty-lines 0)
          ))
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

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; lsp mode
(require 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (setq lsp-prefer-flymake :none)
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

;; diff hl
(require 'diff-hl)
(with-eval-after-load 'diff-hl
  (global-diff-hl-mode)
  (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders nil))

;; Misc
(global-unset-key (kbd "C-z"))

;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Save last opened files
(setq desktop-save nil)
(setq desktop-restore-frames nil)
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
    (monokai-theme github-review helm-projectile company-lsp helm helm-company projectile groovy-mode lsp-mode company yaml-mode yafolding vue-mode undo-tree rust-mode rainbow-delimiters neotree markdown-mode magit json-mode haskell-mode google-translate go-mode diff-hl browse-kill-ring ack))))
