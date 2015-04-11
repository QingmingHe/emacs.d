;;; flycheck-pos-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flycheck-pos-tip" "flycheck-pos-tip.el" (21801
;;;;;;  22473 977031 700000))
;;; Generated autoloads from flycheck-pos-tip.el

(autoload 'flycheck-pos-tip-error-messages "flycheck-pos-tip" "\
Display the tooltip that the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `pos-tip-show-no-propertize', which shows
 the messages in tooltip, depending on the number of lines.

\(fn ERRORS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-pos-tip-autoloads.el ends here
