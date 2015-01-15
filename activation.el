;; desktop should be the first
(starter-kit-load "desktop")
;; Configure org 8.* bundled with Emacs 24.4
(when (> emacs-minor-version 3)
  (starter-kit-load "org"))
(starter-kit-load "eshell")
(starter-kit-load "ibuffer")
(starter-kit-load "ctags")
(starter-kit-load "flycheck")
(starter-kit-load "misc-recommended")
(starter-kit-load "helm")
(starter-kit-load "aspell")
(starter-kit-load "evil")
(starter-kit-load "yasnippet")
(starter-kit-load "autocomplete")
(starter-kit-load "w3m")
(starter-kit-load "dictionary")
(starter-kit-load "company")
(starter-kit-load "bindings")
(starter-kit-load "ansiterm")
;; prog mode should be loaded finally
(starter-kit-load "python")
(starter-kit-load "nxml")
(starter-kit-load "lisp")
(starter-kit-load "cmake")
(starter-kit-load "rst")
(starter-kit-load "fortran")
(starter-kit-load "latex")
(starter-kit-load "graphviz")
(starter-kit-load "matlab")
(starter-kit-load "rnc")
(starter-kit-load "cc")
