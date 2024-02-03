(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(menu-bar-mode -1)

(use-package ace-window
  :ensure
  :bind (("C-x o" . ace-window)
         ("C-x O" . ace-swap-window)))

(use-package dracula-theme
  :ensure
  :demand t
  :config
  (load-theme 'dracula t))

(with-eval-after-load 'doc-view
  (setq doc-view-resolution 300))

(use-package project
  :ensure
  :demand t
  :config
  (setq project-switch-commands 'helm-project))

(use-package magit
  :ensure
  :demand t
  :defer 3)

(use-package yasnippet
  :ensure
  :demand t
  :config
  (yas-global-mode 1))

(use-package git-link
  :ensure
  :bind ("C-c g l" . git-link)
  :init
  (setq git-link-use-commit t))

(use-package company
  :ensure
  :demand t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yafolding
  :ensure
  :demand t
  :config
  (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package lsp-mode
  :ensure
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
         (rust-mode . lsp)))

(use-package undo-tree
  :ensure
  :demand t
  :config
  (setq undo-tree-auto-save-history nil)
  :custom
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :ensure
  :demand t
  :config
  (browse-kill-ring-default-keybindings))

(use-package powerline
  :ensure
  :demand t
  :config
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

(use-package rainbow-delimiters
  :ensure
  :demand t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package helm
  :ensure
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
  :ensure
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
	          (tags-todo "+work" ((org-agenda-files '("~/.org/projects.org")))))
	         ((org-agenda-tag-filter-preset '("+work"))))
	        ("p" "Personal"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	          (tags-todo "+personal" ((org-agenda-files '("~/.org/projects.org")))))
	         ((org-agenda-tag-filter-preset '("+personal"))))
	        ("h" "Hacking"
	         ((agenda "" ((org-agenda-start-day nil) (org-agenda-span 'day)))
	          (tags-todo "+hacking" ((org-agenda-files '("~/.org/projects.org")))))
	         ((org-agenda-tag-filter-preset '("+hacking"))))
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
  :ensure
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
