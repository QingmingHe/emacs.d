;;; helm-swoop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-swoop" "helm-swoop.el" (21802 27396 865036
;;;;;;  400000))
;;; Generated autoloads from helm-swoop.el

(autoload 'helm-swoop-back-to-last-point "helm-swoop" "\


\(fn &optional $CANCEL)" t nil)

(autoload 'helm-swoop "helm-swoop" "\


\(fn &key $QUERY $SOURCE ($multiline current-prefix-arg))" t nil)

(autoload 'helm-swoop-from-isearch "helm-swoop" "\
Invoke `helm-swoop' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-swoop "helm-swoop" "\


\(fn &optional $QUERY $BUFLIST)" t nil)

(autoload 'helm-multi-swoop-all "helm-swoop" "\


\(fn &optional $QUERY)" t nil)

(autoload 'helm-multi-swoop-edit "helm-swoop" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-swoop-pkg.el") (21802 27396 880636
;;;;;;  400000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-swoop-autoloads.el ends here