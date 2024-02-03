;;; adoc-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "adoc-mode" "adoc-mode.el" (0 0 0 0))
;;; Generated autoloads from adoc-mode.el

(autoload 'adoc-mode "adoc-mode" "\
Major mode for editing AsciiDoc text files.
Turning on Adoc mode runs the normal hook `adoc-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.a\\(?:scii\\)?doc\\'" . adoc-mode))

(register-definition-prefixes "adoc-mode" '("adoc-"))

;;;***

;;;### (autoloads nil nil ("adoc-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; adoc-mode-autoloads.el ends here
