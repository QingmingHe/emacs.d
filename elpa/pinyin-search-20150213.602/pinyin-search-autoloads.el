;;; pinyin-search-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pinyin-search" "pinyin-search.el" (21761 49097
;;;;;;  608668 200000))
;;; Generated autoloads from pinyin-search.el

(autoload 'isearch-toggle-pinyin "pinyin-search" "\
Toggle pinyin in searching on or off.
Toggles the value of the variable `pinyin-search-activated'.

\(fn)" t nil)

(autoload 'isearch-forward-pinyin "pinyin-search" "\
Search Chinese forward by Pinyin.

\(fn)" t nil)

(autoload 'isearch-backward-pinyin "pinyin-search" "\
Search Chinese backward by Pinyin.

\(fn)" t nil)
 (define-key isearch-mode-map "\M-sp" #'isearch-toggle-pinyin)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pinyin-search-autoloads.el ends here
