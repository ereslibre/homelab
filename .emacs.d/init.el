(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(with-eval-after-load 'package
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install-selected-packages)
  (package-activate-all))

(defun ereslibre/initialize ()
  ;; Default theme
  (load-theme 'doom-vibrant t)

  ;; Magit
  (require 'magit)

  ;; Yasnippet
  (require 'yasnippet)
  (with-eval-after-load 'yasnippet
    (yas-global-mode 1))

  ;; Doc-view
  (with-eval-after-load 'doc-view
    (setq doc-view-resolution 300))

  ;; Git-link
  (require 'git-link)
  (with-eval-after-load 'git-link
    (global-set-key (kbd "C-c g l") 'git-link)
    (setq git-link-use-commit t))

  ;; Org mode
  (require 'org)
  (with-eval-after-load 'org
    (setq org-agenda-files '("~/org/birthdays.org"
			     "~/org/habits.org"
			     "~/org/inbox.org"
			     "~/org/journal.org"
			     "~/org/notes.org"
			     "~/org/projects.org"
			     "~/org/reminders.org"
			     "~/org/someday.org"
			     "~/org/tickler.org"))
    (setq org-agenda-text-search-extra-files
	  (append (directory-files-recursively "~/org" "\.org$")
		  (directory-files-recursively "~/org" "\.org_archive$")))
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c C-o") 'org-open-at-point)
    (setq org-log-done t)
    (setq org-log-repeat 'note)
    (setq org-agenda-span 'day)
    (setq org-agenda-start-on-weekday nil)
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets '(("~/org/birthdays.org" :maxlevel . 1)
			       ("~/org/habits.org" :maxlevel . 1)
			       ("~/org/projects.org" :maxlevel . 3)
			       ("~/org/someday.org" :maxlevel . 3)
			       ("~/org/tickler.org" :maxlevel . 3)
			       ("~/org/reminders.org" :maxlevel . 3)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-capture-templates
	  '(("t" "TODO"
	     entry (file "~/org/inbox.org")
	     "* TODO %i%?\n  %U"
	     :empty-lines 0)
	    ("T" "Tickler"
	     entry (file "~/org/tickler.org")
	     "* %i%?\n  %U"
	     :empty-lines 0)
	    ("n" "Note"
	     entry (file "~/org/notes.org")
	     "* %?"
	     :empty-lines 0)
	    ("j" "Journal Entry"
	     entry (file+datetree "~/org/journal.org")
	     "* %?"
	     :empty-lines 0)
	    ))
    (setq org-agenda-custom-commands
	  '(("A" "All"
	     ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	      (tags-todo "+personal" ((org-agenda-files '("~/org/projects.org"))))
	      (tags-todo "+work" ((org-agenda-files '("~/org/projects.org"))))
	      (tags-todo "+hacking" ((org-agenda-files '("~/org/projects.org"))))))
	    ("o" "Office"
	     ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	      (tags-todo "+work" ((org-agenda-files '("~/org/projects.org")))))
	     ((org-agenda-tag-filter-preset '("+work"))))
	    ("p" "Personal"
	     ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	      (tags-todo "+personal" ((org-agenda-files '("~/org/projects.org")))))
	     ((org-agenda-tag-filter-preset '("+personal"))))
	    ("h" "Hacking"
	     ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	      (tags-todo "+hacking" ((org-agenda-files '("~/org/projects.org")))))
	     ((org-agenda-tag-filter-preset '("+hacking"))))
	    ("w" "Weekly review"
	     ((agenda "" ((org-agenda-start-day nil)
			  (org-agenda-span 'week)))
	      (tags-todo "+work" ((org-agenda-files '("~/org/projects.org"))))
	      (tags-todo "+personal" ((org-agenda-files '("~/org/projects.org"))))
	      (tags-todo "+hacking" ((org-agenda-files '("~/org/projects.org"))))
	      (tags "+someday" ((org-agenda-files '("~/org/someday.org"))))))))
    (add-to-list 'org-modules 'org-habit)
    (add-hook 'before-save-hook (lambda () (when (eq major-mode 'org-mode) (org-align-tags t)))))

  ;; Company mode
  (with-eval-after-load 'company
    (add-hook 'after-init-hook 'global-company-mode)
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

  ;; Yafolding mode
  (require 'yafolding)
  (with-eval-after-load 'yafolding
    (add-hook 'prog-mode-hook 'yafolding-mode))

  ;; lsp-mode
  (require 'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (setq lsp-auto-configure t)
    (setq lsp-diagnostic-package :none)
    (setq lsp-restart 'auto-restart))
  (add-hook 'prog-mode-hook 'lsp)

  ;; Undo tree
  (require 'undo-tree)
  (with-eval-after-load 'undo-tree
    (global-undo-tree-mode 1))

  ;; Browse kill ring
  (require 'browse-kill-ring)
  (with-eval-after-load 'browse-kill-ring
    (browse-kill-ring-default-keybindings))

  ;; Powerline
  (require 'powerline)
  (with-eval-after-load 'powerline
    (setq powerline-default-separator 'wave)
    (powerline-default-theme))

  ;; Rainbow delimiters
  (require 'rainbow-delimiters)
  (with-eval-after-load 'rainbow-delimiters
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

  ;; helm-lsp
  (require 'helm-lsp)
  (with-eval-after-load 'helm-lsp
    (global-set-key (kbd "C-c w s") 'helm-lsp-workspace-symbol)
    (global-set-key (kbd "C-c w g") 'helm-lsp-global-workspace-symbol))

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

  ;; Frame font
  (set-frame-font "Ubuntu Mono-15:Regular")
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15:Regular"))
  (set-face-attribute 'default t :font "Ubuntu Mono-15:Regular")

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 150 :family "Ubuntu Mono"))))
   '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Ubuntu Mono"))))
   '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Ubuntu Mono"))))
   '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 150 :family "Ubuntu Mono"))))
   '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 150 :family "Ubuntu Mono"))))
   '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Ubuntu Mono"))))
   '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 150 :family "Ubuntu Mono"))))
   '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Ubuntu Mono")))))

  ;; Go
  (setenv "GOPATH" "/home/ereslibre/projects/go")
  (add-to-list 'exec-path "/home/ereslibre/projects/go/bin")

  ;; Cargo
  (add-to-list 'exec-path "/home/ereslibre/.cargo/bin"))

(add-hook 'emacs-startup-hook 'ereslibre/initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default))
 '(delete-selection-mode nil)
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   '("~/projects/org/inbox.org" "~/projects/org/birthdays.org" "~/projects/org/projects.org" "~/projects/org/tickler.org" "~/projects/org/reminders.org" "~/projects/org/someday.org" "~/projects/org/journal.org" "~/projects/org/habits.org"))
 '(package-selected-packages
   '(monokai-theme doom-themes typescript-mode terraform-mode nix-mode adoc-mode helm-lsp protobuf-mode git-link yasnippet powerline helm-projectile helm helm-company projectile groovy-mode lsp-mode company yaml-mode yafolding vue-mode undo-tree rust-mode rainbow-delimiters neotree markdown-mode magit json-mode haskell-mode google-translate go-mode browse-kill-ring ack ripgrep))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 150 :family "Fira Code"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Fira Code"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Fira Code"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 150 :family "Fira Code"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#A1EFE4" :height 150 :family "Fira Code"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 150 :family "Fira Code"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#F92672" :height 150 :family "Fira Code"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 150 :family "Fira Code")))))
