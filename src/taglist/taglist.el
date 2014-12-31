(defun taglist nil
(interactive)
(require 'speedbar)
(let ((tags (speedbar-fetch-dynamic-etags buffer-file-name))
(source-buffer (current-buffer))
(current-line (line-number-at-pos))
(list-pos 0))
(if (get-buffer "*etags tmp*")
(kill-buffer "*etags tmp*"))
(if (get-buffer "*etags list*")
(kill-buffer "*etags list*"))
(set-buffer (get-buffer-create "*etags list*"))
(while tags
(insert "\t")
(insert (buffer-name source-buffer))
(insert " L")
(let ((tag-line
(with-current-buffer source-buffer
(line-number-at-pos (cdar tags)))))
(insert (number-to-string tag-line))
(if (>= current-line tag-line)
(setq list-pos
(1+ list-pos))))
(insert ":\t")
(insert (caar tags))
(insert "\n")
(setq tags (cdr tags)))
(goto-line list-pos)
(setq taglist-window (split-window-vertically))
(set-window-buffer taglist-window "*etags list*")
(select-window taglist-window)
(taglist-mode)))
(defvar taglist-mode-hook nil)
(defvar taglist-keywords
(list (list "^\t\\([^ ]*\\) \\(L[0-9]+\\):\t\\(.*\\)$" 1 font-lock-keyword-face)
(list "^\t\\([^ ]*\\) \\(L[0-9]+\\):\t\\(.*\\)$" 2 font-lock-comment-delimiter-face)
(list "^\t\\([^ ]*\\) \\(L[0-9]+\\):\t\\(.*\\)$" 3 font-lock-function-name-face)))
(defvar taglist-map
(let ((map (make-sparse-keymap)))
(define-key map (kbd "RET") 'taglist-jump)
(define-key map (kbd "q") 'taglist-quit)
map))
(defvar taglist-window nil)
(defun taglist-kill nil
(if (and taglist-window
(window-live-p taglist-window)
(not (one-window-p)))
(delete-window taglist-window))
(setq taglist-window nil)
(kill-buffer "*etags list*"))
(defun taglist-jump nil
(interactive)
(let ((line (buffer-substring
(line-beginning-position)
(line-end-position))))
(string-match "^\t\\([^ ]*\\) L\\([0-9]+\\):\t.*$" line)
(taglist-kill)
(switch-to-buffer (match-string 1 line))
(goto-line (string-to-number (match-string 2 line)))))
(defun taglist-quit nil
(interactive)
(taglist-kill))
(defun taglist-mode nil
(interactive)
(kill-all-local-variables)
(use-local-map taglist-map)
(setq major-mode 'taglist-mode)
(setq mode-name "Tag-List")
(setq font-lock-defaults
(list 'taglist-keywords))
(run-mode-hooks 'taglist-mode-hook))
(provide 'taglist)
