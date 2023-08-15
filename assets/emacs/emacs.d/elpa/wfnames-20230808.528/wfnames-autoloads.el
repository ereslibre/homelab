;;; wfnames-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wfnames" "wfnames.el" (0 0 0 0))
;;; Generated autoloads from wfnames.el

(autoload 'wfnames-setup-buffer "wfnames" "\
Initialize wfnames buffer with FILES and display it with DISPLAY-FN.

Arg DISPLAY-FN default to `switch-to-buffer' if unspecified.
When APPEND is specified, append FILES to existing `wfnames-buffer'.

\(fn FILES &optional (DISPLAY-FN #\\='switch-to-buffer) APPEND)" nil nil)

(register-definition-prefixes "wfnames" '("wfnames-"))

;;;***

;;;### (autoloads nil nil ("wfnames-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wfnames-autoloads.el ends here
