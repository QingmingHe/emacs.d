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

(defun f90-eldoc-should-update (sym sym-plist)
  (let (yes? cache)
    (if (setq cache (gethash sym f90-eldoc-cache))
        (when (time-less-p (plist-get cache :mtime)
                           (plist-get sym-plist :mtime))
          (setq yes? t))
      (setq yes? t))
    yes?))

(defun f90-eldoc-update (sym sym-plist)
  (let (doc)
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
    (string-match
     "[ \t]*\\(function\\|subroutine\\)[ \t]*\\(\\<.+\\>\\)[ \t]*(\\(.*\\))"
     doc)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       font-lock-keyword-face doc)
    (put-text-property (match-beginning 2) (match-end 2) 'face
                       font-lock-function-name-face doc)
    (put-text-property (match-beginning 3) (match-end 3) 'face
                       font-lock-variable-name-face doc)
    (puthash sym (plist-put sym-plist :doc doc) f90-eldoc-cache)))

(defun f90-eldoc-function ()
  (let (sym
        sym-plist
        (sp (syntax-ppss)))
    (unless (nth 4 sp)
      (save-excursion
        (unless (eq 0 (nth 0 sp))
          (goto-char (nth 1 sp)))
        (unless (looking-back "\\(function\\|subroutine\\).*")
          (setq sym (symbol-at-point))
          (setq sym-plist (f90-eldoc-get-sym-plist sym))))
      (if (and
           sym
           sym-plist
           (string-match-p
            "\\(function\\|subroutine\\)" (plist-get sym-plist :doc)))
          (progn
            (when (f90-eldoc-should-update sym sym-plist)
              (f90-eldoc-update sym sym-plist))
            (setq sym-plist (gethash sym f90-eldoc-cache)))
        (setq sym nil sym-plist nil)))
    (plist-get sym-plist :doc)))

(provide 'f90-eldoc)
