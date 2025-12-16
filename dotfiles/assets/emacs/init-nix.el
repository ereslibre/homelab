;; Emacs configuration with Nix-managed packages
;; All packages are provided by Nix, so no package.el or use-package :ensure needed

;; Load use-package for configuration only (packages already installed via Nix)
(require 'use-package)

;; specially on darwin with yabai: this helps identifying the window
;; in order to tile
(menu-bar-mode -1)

(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x O" . ace-swap-window)))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

(use-package project
  :demand t
  :config
  (setq project-switch-commands 'helm-project))

(use-package magit
  :demand t
  :defer 3)

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

(use-package git-link
  :bind ("C-c g l" . git-link)
  :init
  (setq git-link-use-commit t))

(use-package company
  :demand t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package gptel
  :demand t
  :config
  (setq
   gptel-model 'qwen2.5-coder:32b
   gptel-backend (gptel-make-ollama "ollama"
                   :host "hulk.ereslibre.net:11434"
                   :stream t
                   :models '(qwen2.5-coder:32b
                             deepseek-r1:32b))))

(use-package lsp-mode
  :demand t
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :init
  (setq lsp-restart 'interactive)
  :hook (
         (rust-mode . lsp)
         (rust-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (javascript-ts-mode . lsp)
         (python-ts-mode . lsp)
         (go-ts-mode . lsp)
         (bash-ts-mode . lsp)))

;; treesit-auto: Automatically use tree-sitter modes
;; Note: Grammar installation is disabled because grammars are provided by Nix
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install nil) ; Grammars are managed by Nix, don't auto-install
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ; Use tree-sitter for all supported languages
  (global-treesit-auto-mode))

(use-package undo-tree
  :demand t
  :config
  (setq undo-tree-auto-save-history nil)
  :custom
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :demand t
  :config
  (browse-kill-ring-default-keybindings))

(use-package powerline
  :demand t
  :config
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

(use-package rainbow-delimiters
  :demand t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package helm
  :demand t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ([remap occur] . helm-occur)
         ([remap list-buffers] . helm-buffers-list)
         ([remap dabbrev-expand] . helm-dabbrev))
  :config
  (require 'helm-imenu)
  (helm-mode 1)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-i") 'helm-imenu)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action))

(use-package neotree
  :bind (("C-c n" . neotree-toggle)
         ("C-c t" . neotree-find))
  :config
  (setq neo-autorefresh t)
  (setq neo-theme 'ascii))

(use-package org
  :demand t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-o" . org-open-at-point)
         ("C-c ," . org-insert-structure-template))
  :config
  (setq org-directory "~/.org")
  (setq org-agenda-files '("~/.org/habits.org"
			                     "~/.org/inbox.org"
			                     "~/.org/journal.org"
			                     "~/.org/notes.org"
			                     "~/.org/papers.org"
			                     "~/.org/projects.org"
			                     "~/.org/reminders.org"
			                     "~/.org/someday.org"
			                     "~/.org/tickler.org"))
  (setq org-agenda-text-search-extra-files
	      (append (directory-files-recursively "~/.org" "\.org$")
		            (directory-files-recursively "~/.org" "\.org_archive$")))
  (setq org-log-repeat 'note)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (defun update-org-refile-files (ignored)
    (setq org-refile-files (file-expand-wildcards "~/.org/*.org")))
  (update-org-refile-files nil)
  (setq org-refile-targets '((org-refile-files :maxlevel . 3)))
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(g!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")))
  (setq org-capture-templates
	      '(("t" "TODO"
	         entry (file "~/.org/inbox.org")
	         "* TODO %i%?\n  %U"
	         :empty-lines 0)
	        ("T" "Tickler"
	         entry (file "~/.org/tickler.org")
	         "* %i%?\n  %U"
	         :empty-lines 0)
	        ("n" "Note"
	         entry (file "~/.org/notes.org")
	         "* %?"
	         :empty-lines 0)
	        ("j" "Journal Entry"
	         entry (file+datetree "~/.org/journal.org")
	         "* %<%H:%M> %?"
	         :empty-lines 0)
	        ))
  (setq org-agenda-custom-commands
	      '(("A" "All"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
 	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org"))))
	          (tags-todo "+work" ((org-agenda-files '("~/.org/projects.org"))))
	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org"))))))
	        ("o" "Office"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	          (tags-todo "+work" ((org-agenda-files '("~/.org"))))))
	        ("p" "Personal"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org")))))
	         ((org-agenda-tag-filter-preset '("+personal"))))
	        ("h" "Hacking"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org")))))
	         ((org-agenda-tag-filter-preset '("+hacking"))))
	        ("l" "Logbook"
	         ((tags "+logbook" ((org-agenda-files '("~/.org")))))
	         ((org-agenda-tag-filter-preset '("+work"))))
	        ("w" "Weekly review"
	         ((agenda "" ((org-agenda-start-day nil)
			                  (org-agenda-span 'week)))
	          (tags-todo "+work" ((org-agenda-files '("~/.org/projects.org"))))
	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org"))))
	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org"))))
	          (tags "+someday" ((org-agenda-files '("~/.org/someday.org"))))))))
  (require 'org-journal)
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'before-save-hook (lambda ()
                                (when (eq major-mode 'org-mode) (org-align-tags t))))
  (require 'ob-shell)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(use-package sublimity
  :demand t
  :custom
  (sublimity-mode 1)
  :config
  (require 'sublimity-attractive)
  ;; When on org-agenda-mode, tags are aligned first, then sublimity
  ;; resizes and centers, and tags are left out of screen. Redo the
  ;; agenda.
  (add-hook 'sublimity--window-change-functions
            (lambda ()
              (when (eq major-mode 'org-agenda-mode) (org-agenda-redo)))))

;; Load custom file if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
