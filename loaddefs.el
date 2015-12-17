;; This is the autoloads file for packages in ~/.emacs.d/src which is
;; installed manually and have no auto-generated *-autoloads.el file

(autoload 'auto-capitalize-mode "auto-capitalize"
  "Toggle `auto-capitalize' minor mode in this buffer.  With optional prefix
ARG, turn `auto-capitalize' mode on iff ARG is positive.  This sets
`auto-capitalize' to t or nil (for this buffer) and ensures that
`auto-capitalize' is installed in `after-change-functions' (for all buffers)."
  t)
(autoload 'texmathp "texmathp"
  "Determine if point is inside (La)TeX math mode.  Returns t or nil.
Additional info is placed into `texmathp-why'.  The functions assumes that you
have (almost) syntactically correct (La)TeX in the buffer.  See the variable
`texmathp-tex-commands' about which commands are checked." t)
(autoload 'texmathp-match-switch "texmathp"
  "Search backward for any of the math switches.  Limit searched to BOUND.")
(autoload 'c-turn-on-eldoc-mode "c-eldoc"
  "Show function API in the minibuffer." t)
(autoload 'ahk-mode "ahk-mode"
  "Mode for editing AutoHotKey configuration file." t)
(autoload 'rnc-mode "rnc-mode" "Rnc mode" t)
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'sdcv-search-pointer "sdcv" "Translate word in other buffer." t)
(autoload 'sdcv-search-pointer+ "sdcv" "Translate word in tooltip." t)
(autoload 'sdcv-search-input "sdcv" "Translate word in other buffer." t)
(autoload 'sdcv-search-input+ "sdcv" "Translate word in tooltip." t)

;; `helm-insert-latex-math' need `LaTeX-math-mode' which is not autoloaded by
;; default.
(autoload 'LaTeX-math-mode "latex" "\
A minor mode with easy access to TeX math macros.

Easy insertion of LaTeX math symbols.  If you give a prefix argument, the
symbols will be surrounded by dollar signs.  The following commands are
defined:

\\{LaTeX-math-mode-map}" t nil)
(autoload 'rst-mode "rst")
(autoload 'eim-use-package "eim" "Another emacs input method" t)
(autoload 'eim-insert-ascii "eim-extra" "Use a key to insert English." t)
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "\
Wrap the buffer text with adaptive filling.

\(fn &optional ARG)" t nil)
