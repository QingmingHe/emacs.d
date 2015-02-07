;;; f90-eldoc.el --- Show API of Fortran functions through eldoc.
;;
;; Copyright (C) 2015 Qingming He
;;
;; Author: Qingming He <906459647@qq.com>
;; Keywords: convenience
;; Version: dev
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; To use it, you should:
;; 1. Install GNU global compiled with CTAGS support.
;; 2. Copy gtags.conf to ~/.glocalrc.
;; 3. Run gtags at your project root like this:
;;    gtags --gtagslabel=ctags --gtagsconf=~/.globalrc
;;
;; Then, add to your ~/.emacs or ~/.emacs.d/init.el:
;; (require 'f90-eldoc)
;; (add-hook 'f90-mode-hook 'f90-turn-on-eldoc-mode)
;; Or if you only want to turn it on for your project `f90-mode' buffers,
;; create a file .dir-locals at project root and write to it:
;; ((f90-mode . ((eval . (f90-turn-on-eldoc-mode)))))
;;
;;; Code:

(require 'f90)
(require 'eldoc)

(defvar f90-eldoc-global-exec (executable-find "global")
  "Executable of GNU global.")

(defvar f90-eldoc-cache (make-hash-table :test 'equal)
  "F90 eldoc hash table cache. The keys of table are function or subroutine names
(symbols), the corresponding value is a plist containing \":line\", \":file\",
\"doc\", \":mtime\" properties. Value of these properties are:

\":line\": at which line is the function or subroutine defined.
\":file\": in which file is the function or subroutine defined.
\":doc\": API documentation of the function or subroutine.
\":mtime\": the last time the \":file\" was modified.")

(defun f90-eldoc-get-sym-plist (sym)
  "Get plist for given SYM by GNU global. For detailed description of the
plist and SYM, see `f90-eldoc-cache'.

The \":doc\" obtained in this function has no properties which are added in
`f90-eldoc-font-lock'. If more than one definitions are found for SYM, only
the first one is returned."
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
  "Whether API documentation for SYM should be updated. If should, return t,
otherwise return nil. The condition for updating is SYM not in
`f90-eldoc-cache' or \":file\" in `f90-eldoc-cache' is older than the current
one. This is realized by comparing \":mtime\"."
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

(defun f90-eldoc-update (sym kw-or-i)
  "Update API documentation for SYM. KW-OR-I is key word of argument of index
of argument at point."
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
        (setq sym-plist (f90-eldoc-font-lock sym-plist kw-or-i))
        (puthash sym sym-plist f90-eldoc-cache))
      sym-plist)))

(defun f90-eldoc-font-lock (sym-plist kw-or-i)
  "Add text properties to API documentation. SYM-PLIST is a plist for SYM, see
also `f90-eldoc-cache'. KW-OR-I is key word of argument of index of argument
at point.

All words before function or subroutine name are put `font-lock-keyword-face';
function or subroutine name is put `font-lock-function-name-face'; the
argument at point `bold'; other arguments `font-lock-variable-name-face'."
  (when sym-plist
    (let (last-pos
          current-arg-index
          current-kword)
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
        (while (re-search-forward "\\([a-zA-Z0-9_]+\\)[ \t]*[,)]" nil t)
          (setq current-kword (match-string 1))
          (setq current-arg-index (1+ current-arg-index))
          (cond ((and
                  (stringp kw-or-i)
                  (string= kw-or-i (downcase current-kword)))
                 (put-text-property last-pos (1- (point)) 'face 'bold))
                ((and
                  (number-or-marker-p kw-or-i)
                  (= current-arg-index kw-or-i))
                 (put-text-property last-pos (1- (point)) 'face 'bold))
                (t
                 (put-text-property last-pos (1- (point)) 'face
                                    font-lock-variable-name-face)))
          (setq last-pos (point)))
        (setq sym-plist (plist-put sym-plist :doc (buffer-string))))
      sym-plist)))

(defun f90-eldoc-args-index-or-kw (beg)
  "Get key word argument or index of argument at point. BEG is the beginning
of the argument list which should be the point after function or subroutine
name. If a key word found, return the key word, otherwise return the index of
argument at point."
  (let (lbnd rbnd index kword)
    (save-excursion
      (setq lbnd (re-search-backward "[,(]" beg t)))
    (save-excursion
      (setq rbnd (re-search-forward "[,)]" nil t)))
    (when (and lbnd rbnd)
      (save-excursion
        (goto-char lbnd)
        (when (re-search-forward "[ \t]*\\([a-zA-Z0-9_]+\\)[& \t\n]*=" rbnd t)
          (setq kword (downcase (match-string 1))))))
    (unless kword
      (setq index 1)
      (save-excursion
        (while (re-search-backward "[a-zA-Z0-9_]+[ \t]*\\((.*)\\)?[ \t]*," beg t)
          (setq index (1+ index)))))
    (or kword index)))

(defun f90-eldoc-function ()
  "Return a API documentation for current context or nil. Following conditions
should be satisfied to return an API doc:
1. Point not in comment;
2. Point not in function or subroutine definition context;
3. Point not within function or subroutine usage context;
4. A function or subroutine name can be found."
  (let (sym
        sym-plist
        kw-or-i
        (sp (syntax-ppss)))
    (unless (nth 4 sp)
      (save-excursion
        (unless (eq 0 (nth 0 sp))
          (goto-char (nth 1 sp)))
        (unless (looking-back "\\(function\\|subroutine\\).*")
          (setq sym (symbol-at-point))))
      (when (and sym (not (eq 0 (nth 0 sp))))
        (setq kw-or-i (f90-eldoc-args-index-or-kw (nth 1 sp))))
      (if (f90-eldoc-should-update sym)
          (setq sym-plist (f90-eldoc-update sym kw-or-i))
        (setq sym-plist
              (f90-eldoc-font-lock
               (gethash sym f90-eldoc-cache)
               kw-or-i))))
    (plist-get sym-plist :doc)))

(defun f90-turn-on-eldoc-mode ()
  "Turn on eldoc mode for `f90-mode'. Set buffer local
`eldoc-documentation-function' to be `f90-eldoc-function'."
  (interactive)
  (when (eq major-mode 'f90-mode)
    (eldoc-mode 1)
    (setq-local eldoc-documentation-function 'f90-eldoc-function)))

(provide 'f90-eldoc)

;;; f90-eldoc.el ends here
