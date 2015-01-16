;; the basics
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'popup)
(require 'ido)
(require 'smex)
(require 'dash)
(unless *terminal*
  (require 'pos-tip))

(require 'server)
(require 'package)
(require 'company)
(require 'gnus)
(require 'flycheck)
(require 'flycheck-fortran-gfortran-zh)
(require 'smartparens)
(require 'smartparens-config)
(require 'rainbow-delimiters)
(require 'flx-ido)
(require 'dired)
(require 'dired-x)
(require 'ibuffer-vc)
;; dired+ is required in starter-kit-misc.org for that something should be set
;; before loading dired+
(require 'bookmark+)
(require 'linum-relative)
(require 'undo-tree)
(require 'expand-region)
(require 'window-numbering)
(require 'yasnippet)
(require 'goto-chg)
(require 'etags-select)
(require 'imenu)
(require 'imenu+)
(require 'ace-jump-mode)
(require 'git-rebase-mode)
(require 'git-commit-mode)
(require 'magit)
(require 'dictionary)
(require 'no-word)
(require 'remember)

;; eshell, term
(require 'em-cmpl)
(require 'em-prompt)
(require 'em-term)
(require 'term)
(require 'multi-eshell)

;; my own package for handling projects, including project.el,
;; project-root.el, etags-update.el
(require 'project)
;; downloaded somewhere
(require 'taglist)

;; color theme, font ...
(require 'color-theme)
(require 'color-theme-molokai)
(require 'color-theme-solarized)
(require 'highlight-indentation)
; powerline is loaded after setting color-theme, otherwise some errors occur

;; auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang)
; I've done some hack on etags
(require 'auto-complete-etags)
(require 'auto-complete-gtags)
(require 'ring)
(require 'epc)
(require 'python-environment)
(require 'jedi)

;; helm
(require 'helm)
(require 'helm-config)
(require 'helm-gtags)

;; prog mode
(require 'cmake-mode)
(require 'cython-mode)
(require 'clojure-mode)
(require 'graphviz-dot-mode)
(require 'f90-interface-browser)
(require 'clojure-mode)
(require 'csv-mode)
(require 'gnuplot)
(require 'matlab)
(require 'rnc-mode)

;; docs modes
(require 'markdown-mode)
(require 'markdown-mode+)
(require 'rst)

;;TODO auctex
(require 'cdlatex)

;; org is loaded in init.el or starter-kit-org.org
;; w3m is loaded optionally in starter-kit-w3m.org
;; evil and evil-* are handled in starter-kit-evil.org
