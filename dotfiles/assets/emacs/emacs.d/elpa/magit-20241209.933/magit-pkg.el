;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20241209.933"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.3")
    (seq           "2.24")
    (transient     "0.8.0")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "873da4a62dc609ff0c8a7e0ec417b263e3ed2269"
  :revdesc "873da4a62dc6"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
