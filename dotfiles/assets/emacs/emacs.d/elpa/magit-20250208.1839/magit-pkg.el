;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250208.1839"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.0")
    (magit-section "4.3.0")
    (seq           "2.24")
    (transient     "0.8.4")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "085baa2ac96df90bdb2a1bedd3598df19919d731"
  :revdesc "085baa2ac96d"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
