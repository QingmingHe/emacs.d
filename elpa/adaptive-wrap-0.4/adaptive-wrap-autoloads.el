;;; adaptive-wrap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "adaptive-wrap" "adaptive-wrap.el" (21794 20510
;;;;;;  0 0))
;;; Generated autoloads from adaptive-wrap.el

(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "\
Wrap the buffer text with adaptive filling.

\(fn &optional ARG)" t nil)

(easy-menu-add-item menu-bar-options-menu '("Line Wrapping in This Buffer") ["Adaptive Wrap" (lambda nil (interactive) (if adaptive-wrap-prefix-mode (adaptive-wrap-prefix-mode -1) (adaptive-wrap-prefix-mode 1))) :visible (menu-bar-menu-frame-live-and-visible-p) :help "Show wrapped long lines with an adjustable prefix" :style toggle :selected adaptive-wrap-prefix-mode])

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; adaptive-wrap-autoloads.el ends here
