;;;

(require 'f90)
(require 'eldoc)

(defvar f90-eldoc-global-exec (executable-find "global")
  "Executable of global.")

(setq f90-eldoc-cache (make-hash-table :test 'equal))

(defun f90-eldoc-get-sym-plist (sym)
  (let (global-out sym-plist fn-tag)
    (when sym
      (setq global-out
            (shell-command-to-string
             (format "%s -xa %s" f90-eldoc-global-exec sym)))
      (unless (string-empty-p global-out)
        (setq fn-tag
              (catch 'fn-tag
                (unless
                    (mapc
                     (lambda (tag)
                       (when (string-match-p "\\(function\\|subroutine\\)" tag)
                         (setq global-out (split-string tag))
                         (throw 'fn-tag tag)))
                     (split-string global-out "\n"))
                  nil)))
        (when fn-tag
          (setq sym-plist
                (plist-put sym-plist
                           :line
                           (string-to-int (nth 1 global-out))))
          (setq sym-plist
                (plist-put sym-plist
                           :file
                           (nth 2 global-out)))
          (setq sym-plist
                (plist-put sym-plist
                           :doc
                           (mapconcat 'concat (nthcdr 3 global-out) " ")))
          (setq sym-plist
                (plist-put sym-plist
                           :mtime
                           (nth 5 (file-attributes (nth 2 global-out))))))))
    sym-plist))

(defun f90-eldoc-should-update (sym)
  (let (yes? sym-plist)
    (setq sym-plist (gethash sym f90-eldoc-cache))
    (when (or
           (and sym (not sym-plist))
           (and sym-plist
                (time-less-p
                 (plist-get sym-plist :mtime)
                 (nth 5 (file-attributes (plist-get sym-plist :file))))))
      (setq yes? t))
    yes?))

(defun f90-eldoc-update (sym arg-index)
  (when sym
    (let (doc sym-plist)
      (setq sym-plist (f90-eldoc-get-sym-plist sym))
      (when sym-plist
        (if (string-match-p "&" (plist-get sym-plist :doc))
            (with-temp-buffer
              (erase-buffer)
              (goto-char (point-min))
              (insert-file-contents (plist-get sym-plist :file))
              (goto-char (point-min))
              (forward-line (1- (plist-get sym-plist :line)))
              (while (looking-at ".*&[ \t]*$")
                (f90-join-lines t))
              (setq
               doc
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (setq doc (plist-get sym-plist :doc)))
        (setq sym-plist (plist-put sym-plist :doc doc))
        (setq sym-plist (f90-eldoc-font-lock sym-plist arg-index))
        (puthash sym sym-plist f90-eldoc-cache))
      sym-plist)))

(defun f90-eldoc-font-lock (sym-plist arg-index)
  (when sym-plist
    (let (last-pos
          current-arg-index
          (arg-index (or arg-index -1)))
      (with-temp-buffer
        (erase-buffer)
        (goto-char (point-min))
        (insert (plist-get sym-plist :doc))
        (goto-char (point-min))
        (setq last-pos (point))
        (when (re-search-forward "\\(function\\|subroutine\\)" nil t)
          (put-text-property last-pos (point) 'face font-lock-keyword-face))
        (setq last-pos (point))
        (when (re-search-forward "(" nil t)
          (put-text-property last-pos (1- (point))
                             'face font-lock-function-name-face))
        (setq last-pos (point))
        (setq current-arg-index 0)
        (while (re-search-forward "[,)]" nil t)
          (setq current-arg-index (1+ current-arg-index))
          (if (= current-arg-index arg-index)
              (put-text-property last-pos (1- (point)) 'face 'bold)
            (put-text-property last-pos (1- (point)) 'face
                               font-lock-variable-name-face))
          (setq last-pos (point)))
        (setq sym-plist (plist-put sym-plist :doc (buffer-string))))
      sym-plist)))

(defun f90-eldoc-args-index-or-kw (beg end)
  (let (lbnd rbnd index kword)
    (save-excursion
      (setq lbnd (re-search-backward "[,(]" beg t)))
    (save-excursion
      (setq rbnd (re-search-forward "[,)]" end t)))
    (when (and lbnd rbnd)
      (save-excursion
        (goto-char lbnd)
        (when (re-search-forward "[ \t]*\\([a-zA-Z0-9_]+\\).*=" rbnd nil)
          (setq kword (downcase (match-string 1))))))
    (unless kword
      (setq index 0)
      (save-excursion
        (while (re-search-backward "\\((.*)\\)?[ \t]*[,(]" beg t)
          (setq index (1+ index)))))
    (or kword index)))

(defun f90-eldoc-args-index (beg)
  (when beg
    (let ((index 0))
      (save-excursion
        (while (re-search-backward "\\((.*)\\)?[ \t]*[,(]" beg t)
          (setq index (1+ index))))
      index)))

(defun f90-eldoc-function ()
  (let (sym
        sym-plist
        args-index
        (sp (syntax-ppss)))
    (unless (nth 4 sp)
      (save-excursion
        (unless (eq 0 (nth 0 sp))
          (goto-char (nth 1 sp)))
        (unless (looking-back "\\(function\\|subroutine\\).*")
          (setq sym (symbol-at-point))))
      (when (and sym (not (eq 0 (nth 0 sp))))
        (setq args-index (f90-eldoc-args-index (nth 1 sp))))
      (if (f90-eldoc-should-update sym)
          (setq sym-plist (f90-eldoc-update sym args-index))
        (setq sym-plist
              (f90-eldoc-font-lock
               (gethash sym f90-eldoc-cache)
               args-index))))
    (plist-get sym-plist :doc)))

(provide 'f90-eldoc)
