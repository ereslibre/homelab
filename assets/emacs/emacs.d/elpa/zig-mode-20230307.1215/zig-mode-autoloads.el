;;; zig-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zig-mode" "zig-mode.el" (0 0 0 0))
;;; Generated autoloads from zig-mode.el

(autoload 'zig-compile "zig-mode" "\
Compile using `zig build`." t nil)

(autoload 'zig-build-exe "zig-mode" "\
Create executable from source or object file." t nil)

(autoload 'zig-build-lib "zig-mode" "\
Create library from source or assembly." t nil)

(autoload 'zig-build-obj "zig-mode" "\
Create object from source or assembly." t nil)

(autoload 'zig-test-buffer "zig-mode" "\
Test buffer using `zig test`." t nil)

(autoload 'zig-run "zig-mode" "\
Create an executable from the current buffer and run it immediately." t nil)
 (autoload 'zig-format-buffer "current-file" nil t)
 (autoload 'zig-format-region "current-file" nil t)
 (autoload 'zig-format-on-save-mode "current-file" nil t)

(autoload 'zig-mode "zig-mode" "\
A major mode for the Zig programming language.

\\{zig-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-mode))

(register-definition-prefixes "zig-mode" '("zig-"))

;;;***

;;;### (autoloads nil nil ("zig-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zig-mode-autoloads.el ends here
