;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20241126.1900"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.2")
    (seq           "2.24")
    (transient     "0.7.8")
    (with-editor   "3.4.2"))
  :url "https://github.com/magit/magit"
  :commit "e52dedf07f7726528cd006db88235c23a481351d"
  :revdesc "e52dedf07f77"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
