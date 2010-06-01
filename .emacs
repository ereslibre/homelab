; REQUIREMENTS
;     * maxframe
;     * color-theme
;     * zenburn
;     * yasnippet
;     * xcscope
;     * pastebin

; add load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/erc-5.3")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
; miscellaneous
(column-number-mode 1)
(require 'highlight-current-line)
(highlight-current-line-on t) ; highlight current line
; auto detect file coding style
(require 'dtrt-indent)
(dtrt-indent-mode 1)
; enable gui wellness
(tool-bar-mode nil)
(menu-bar-mode nil)
(scroll-bar-mode nil)
; maximize the window when loading
;(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t)
; set default font family and size
;; (custom-set-faces
;;  '(default ((t (:height 68 :family "Monaco")))))
(custom-set-faces
 '(default ((t (:height 120 :family "Monaco")))))
; set some personal data
(setq user-mail-address "ereslibre@ereslibre.es")
(setq user-full-name "Rafael Fernández López")
; make it pretty colors
(require 'color-theme)
(color-theme-initialize)
(color-theme-emacs-nw)
; (color-theme-xemacs)
; (color-theme-jsc-light)
; (require 'zenburn)
; (color-theme-zenburn)
(custom-set-variables
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(show-paren-mode t)
 '(transient-mark-mode t))
(desktop-save-mode 1) ; save the desktop when leaving
(setq backup-inhibited t)
(setq auto-save-default nil)
; some default indentation follows
(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq objc-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq default-tab-width 4)
; useful shortcuts
(global-set-key "\C-l" 'goto-line)
; set default compile command
(setq compile-command "cd ~/proyectos/ideallibrary ; ./waf")
; yasnippet
(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(add-hook 'yas/after-exit-snippet-hook
		  '(lambda ()
			 (indent-region yas/snippet-beg yas/snippet-end))) ; indent when finishing snippet
(require 'xcscope) ; xcscope
; pastebin
(require 'pastebin)
(setq pastebin-identity "ereslibre")

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
							  auto-mode-alist))
