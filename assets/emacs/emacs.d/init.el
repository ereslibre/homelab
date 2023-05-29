(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(menu-bar-mode -1)

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

(with-eval-after-load 'doc-view
  (setq doc-view-resolution 300))

(use-package magit
  :defer 3)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package git-link
  :bind ("C-c g l" . git-link)
  :init
  (setq git-link-use-commit t))

(use-package company
  :demand
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yafolding
  :config
  (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package lsp-mode
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-auto-configure t)
  (setq lsp-restart 'interactive)
  (add-to-list 'lsp-language-id-configuration '(rego-mode . "rego"))
  (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "el"))
  (add-hook 'prog-mode-hook 'lsp))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package powerline
  :config
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package helm
  :demand
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

(use-package projectile
  :demand
  :bind-keymap ("C-c p" . projectile-command-map)
  :after (helm)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories (append '(".svn" ".git" ".hg" ".repo" ".vagrant" "build") projectile-globally-ignored-directories)))

(use-package neotree
  :bind (("C-c n" . neotree-toggle)
         ("C-c t" . neotree-find))
  :config
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
	      (message "Could not find git project root.")))))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-o" . org-open-at-point))
  :config
  (setq org-agenda-files '("~/.org/habits.org"
			                     "~/.org/inbox.org"
			                     "~/.org/journal.org"
			                     "~/.org/notes.org"
			                     "~/.org/projects.org"
			                     "~/.org/reminders.org"
			                     "~/.org/someday.org"
			                     "~/.org/tickler.org"))
  (setq org-agenda-text-search-extra-files
	      (append (directory-files-recursively "~/.org" "\.org$")
		            (directory-files-recursively "~/.org" "\.org_archive$")))
  (setq org-log-done 'note)
  (setq org-log-repeat 'note)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '(("~/.org/habits.org" :maxlevel . 1)
                             ("~/.org/notes.org" :maxlevel . 3)
			                       ("~/.org/projects.org" :maxlevel . 3)
			                       ("~/.org/someday.org" :maxlevel . 3)
			                       ("~/.org/tickler.org" :maxlevel . 3)
			                       ("~/.org/reminders.org" :maxlevel . 3)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
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
	         "* %?"
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
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'before-save-hook (lambda ()
                                (when (eq major-mode 'org-mode) (org-align-tags t)))))

(use-package sublimity
  :demand
  :config
  (require 'sublimity-attractive)
  (sublimity-mode 1)
  ;; When on org-agenda-mode, tags are aligned first, then sublimity
  ;; resizes and centers, and tags are left out of screen. Redo the
  ;; agenda.
  (add-hook 'sublimity--window-change-functions
            (lambda ()
              (when (eq major-mode 'org-agenda-mode) (org-agenda-redo)))))
