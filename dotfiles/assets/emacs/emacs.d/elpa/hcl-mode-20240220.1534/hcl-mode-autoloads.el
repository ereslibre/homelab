;;; hcl-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from hcl-mode.el

(autoload 'hcl-mode "hcl-mode" "\
Major mode for editing hcl configuration file

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.hcl\\'" . hcl-mode))
(add-to-list 'auto-mode-alist '("\\.nomad\\'" . hcl-mode))
(register-definition-prefixes "hcl-mode" '("hcl-"))


;;; End of scraped data

(provide 'hcl-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; hcl-mode-autoloads.el ends here
