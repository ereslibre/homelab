(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))
