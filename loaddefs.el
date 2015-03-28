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
