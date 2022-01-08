;;; rego-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rego-mode" "rego-mode.el" (0 0 0 0))
;;; Generated autoloads from rego-mode.el

(add-to-list 'auto-mode-alist '("\\.rego\\'" . rego-mode))

(autoload 'rego-mode "rego-mode" "\
Major mode for editing Rego files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rego-mode" '("rego-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rego-mode-autoloads.el ends here
