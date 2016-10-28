;;; doc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "doc-mode" "doc-mode.el" (22543 27006 458274
;;;;;;  36000))
;;; Generated autoloads from doc-mode.el

(autoload 'doc-mode-next-template "doc-mode" "\
Jump to the next unfinished documentation template in this buffer.

\(fn &optional POS LIMIT)" t nil)

(autoload 'doc-mode-previous-template "doc-mode" "\
Jump to the previous unfinished documentation template in this buffer.

\(fn &optional POS LIMIT)" t nil)

(autoload 'doc-mode-first-template "doc-mode" "\
Jump to the first unfinished documentation template in this buffer.

\(fn)" t nil)

(autoload 'doc-mode "doc-mode" "\
Minor mode for editing in-code documentation.

\(fn &optional ARG)" t nil)

(autoload 'doc-mode-remove-tag-doc "doc-mode" "\
Remove the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-fix-tag-doc "doc-mode" "\


\(fn TAG)" t nil)

(defalias 'doc-mode-add-tag-doc 'doc-mode-fix-tag-doc)

(autoload 'doc-mode-check-tag-doc "doc-mode" "\


\(fn TAG &optional PRINT-MESSAGE-P)" t nil)

(autoload 'doc-mode-check-buffer "doc-mode" "\


\(fn)" t nil)

(autoload 'doc-mode-next-faulty-doc "doc-mode" "\
Jump to the next faulty documentation and print error.

\(fn)" t nil)

(autoload 'doc-mode-fold-tag-doc "doc-mode" "\
Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-unfold-doc "doc-mode" "\
Unfold the comment before POINT.

\(fn POINT)" t nil)

(autoload 'doc-mode-unfold-tag-doc "doc-mode" "\
Unfold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

(autoload 'doc-mode-fold-all "doc-mode" "\


\(fn &optional ARG)" t nil)

(autoload 'doc-mode-unfold-all "doc-mode" "\


\(fn)" t nil)

(autoload 'doc-mode-toggle-tag-doc-folding "doc-mode" "\
Toggle folding of TAG's documentation.
If called interactively, use the tag given by `doc-mode-current-tag'.

\(fn TAG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; doc-mode-autoloads.el ends here
