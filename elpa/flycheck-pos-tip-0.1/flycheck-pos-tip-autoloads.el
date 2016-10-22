;;; flycheck-pos-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flycheck-pos-tip" "flycheck-pos-tip.el" (22539
;;;;;;  33819 886163 92000))
;;; Generated autoloads from flycheck-pos-tip.el

(autoload 'flycheck-pos-tip-error-messages "flycheck-pos-tip" "\
Display ERRORS in a graphical tooltip.

\(fn ERRORS)" nil nil)

(defvar flycheck-pos-tip-mode nil "\
Non-nil if Flycheck-Pos-Tip mode is enabled.
See the command `flycheck-pos-tip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flycheck-pos-tip-mode'.")

(custom-autoload 'flycheck-pos-tip-mode "flycheck-pos-tip" nil)

(autoload 'flycheck-pos-tip-mode "flycheck-pos-tip" "\
A minor mode to show Flycheck error messages in a popup.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-pos-tip-autoloads.el ends here
