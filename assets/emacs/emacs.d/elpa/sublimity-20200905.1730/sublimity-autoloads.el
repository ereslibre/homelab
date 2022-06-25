;;; sublimity-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sublimity" "sublimity.el" (0 0 0 0))
;;; Generated autoloads from sublimity.el

(defvar sublimity-mode nil "\
Non-nil if Sublimity mode is enabled.
See the `sublimity-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sublimity-mode'.")

(custom-autoload 'sublimity-mode "sublimity" nil)

(autoload 'sublimity-mode "sublimity" "\
smooth-scrolling and minimap, like sublime editor

This is a minor mode.  If called interactively, toggle the
`Sublimity mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='sublimity-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "sublimity" '("sublimity-"))

;;;***

;;;### (autoloads nil "sublimity-attractive" "sublimity-attractive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sublimity-attractive.el

(register-definition-prefixes "sublimity-attractive" '("split-window" "sublimity-attractive-"))

;;;***

;;;### (autoloads nil "sublimity-map" "sublimity-map.el" (0 0 0 0))
;;; Generated autoloads from sublimity-map.el

(register-definition-prefixes "sublimity-map" '("sublimity-map-"))

;;;***

;;;### (autoloads nil "sublimity-scroll" "sublimity-scroll.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sublimity-scroll.el

(register-definition-prefixes "sublimity-scroll" '("sublimity-scroll-"))

;;;***

;;;### (autoloads nil nil ("sublimity-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sublimity-autoloads.el ends here
