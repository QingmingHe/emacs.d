;;; project-root.el --- Define a project root and take actions based upon it.

;; Copyright (C) 2008-2010 Philip Jackson, Alexander Solovyov, Vladimir Sidorenko

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Author: Alexander Solovyov <piranha@piranha.org.ua>
;; Author: Vladimir Sidorenko <yoyavova@gmail.com>
;; Version: 20110206.1230
;; X-Original-Version: 0.8-pre

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; project-root.el allows the user to create rules that will identify
;; the root path of a project and then run an action based on the
;; details of the project.
;;
;; Example usage might be might be that you want a certain indentation
;; level/type for a particular project.
;;
;; once project-root-fetch has been run `project-details' will either
;; be nil if nothing was found or the project name and path in a cons
;; pair.

;; An example configuration:

;; :root-contains-files is necessary.
;; (setq project-roots
;;       `(("GPerl"
;;          :root-contains-files ("t" "lib")
;;          :filename-regex ,(regexify-ext-list '(pl pm))
;;          :on-hit (lambda (p) (message (car p))))
;;         ("Django"
;;          :root-contains-files ("manage.py")
;;          :filename-regex ,(regexify-ext-list '(py html css js))
;;          :exclude-paths ("media" "contrib"))
;;         ("SUGAR"
;;          :root-contains-files ("SUGAR-TAGS")
;;          :filename-regex ,(regexify-ext-list '(py f90 f))
;;          :exclude-paths (".git" "tests")
;;          :gfortran-include-paths ("../BUILD_DEBUG/include"))))
;;
;; This defines one project called "Generic Perl Projects" by running
;; the tests path-matches and root-contains-files. Once these tests
;; have been satisfied and a project found then (the optional) :on-hit
;; will be run.

;;; The tests:

;; :path-matches maps to `project-root-path-matches' and
;; :root-contains-files maps to `project-root-upward-find-files'. You
;; can use any amount of tests.

;;; Configuration:

;; :filename-regex should contain regular expression, which is passed
;;  to `find` to actually find files for your project.
;; :exclude-paths can contain paths to omit when searching for files.

;;; installation:

;; Put this file into your `load-path' and evaulate (require
;; 'project-root).

;;; Using yourself:

;; If you wrap a call in `with-project-root' then everything in its
;; body will execute under project root:
;;
;; (with-project-root
;;  (shell-command-to-string "pwd"))

(require 'cl)

(defun project-root-find-prune (paths &optional no-default-directory)
  (mapconcat (lambda (path)
               (if no-default-directory
                   (concat " -path \"" path "\" -prune ")
                 (concat " -path \"" default-directory path "\" -prune ")))
             paths "-o"))

(defvar project-root-name-split "@"
  "Split of name and root of `project-root-seen-projects'.")

(defvar project-root-rep-paths
  '(".git" ".hg" ".svn")
  "Repository paths at project root.")

(defvar project-root-extra-find-args
  (project-root-find-prune project-root-rep-paths t)
  "Extra find args that will be AND'd to the defaults (which are
in `project-root-file-find-process')")

(defvar project-root-seen-projects nil
  "All of the projects that we have met so far in this session.")

(defvar project-root-file-cache nil
  "Cache for `completing-read'")

(make-variable-buffer-local
 (defvar project-details nil
  "The name and path of the current project root."))

(defvar project-root-test-dispatch
  '((:root-contains-files . project-root-upward-find-files)
    (:path-matches . project-root-path-matches))
  "Map a property name to root test function.")

(defvar project-roots nil
  "An alist describing the projects and how to find them.")

(defvar project-roots-cache nil
  "An alist containing other project data for inner usage.")

(defvar project-root-max-search-depth 20
  "Don't go any further than this many levels when searching down
a filesystem tree")

(defvar project-root-find-options
  ""
  "Extra options to pass to `find' when using project-root-find-file.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

(defvar project-root-storage-file "~/.emacs.d/.project-roots"
  "File, where seen projects info is saved.")

(defun regexify-ext-list (extensions)
  "Turn a list of extensions to a regexp."
  (concat ".*\\.\\(" (mapconcat (lambda (x) (format "%s" x))
                                extensions "\\|") "\\)\\'"))

(defvar project-root-file-regexp
  (let ((maps
         '(org md rst txt c cc cxx cpp c++ h hpp hxx java f F for FOR f77 F77
               f90 F90 f95 F95 f03 F03 f08 F08 py pl rb el lisp cl scheme mk)))
    (if (executable-find "ctags")
        (regexify-ext-list
         (with-temp-buffer
           (call-process "ctags" nil t nil "--list-maps")
           (goto-char (point-min))
           (while (re-search-forward "\\*\\.\\([^ \t\n]+\\)[ \t\n]" nil t)
             (add-to-list 'maps (intern (match-string 1))))
           maps))
      (regexify-ext-list maps)))
  "File regexp that will be recognized as a project file.")

(defun project-root-path-matches (re)
  "Apply RE to the current buffer name returning the first
match."
  (let ((filename (or buffer-file-name (expand-file-name "c.c"))))
    (when (and filename (string-match re filename))
      (match-string 1 filename))))

(defun project-root-get-root (project)
  "Fetch the root path of the project according to the tests
described in PROJECT."
  (let (root new-root)
    (catch 'not-a-project
      (mapc
       (lambda (test)
         (when (plist-get project (car test))
           ;; grab a potentially different root
           (setq new-root
                 (funcall (cdr test) (plist-get project (car test))))
           (cond
            ((null new-root)
             (throw 'not-a-project nil))
            ;; check root is so far consistent
            ((and (not (null root))
                  (not (string= root new-root)))
             (throw 'not-a-project nil))
            (t
             (setq root new-root)))))
       project-root-test-dispatch))
    (when root
      (file-name-as-directory root))))

(defun project-root-data (key &optional project)
  "Grab the value (if any) for key in PROJECT.

If PROJECT is omitted then attempt to get the value for the current project."
  (let ((p (or project project-details))
        (sp project-root-name-split)
        val)
    (setq val
          (or
           (plist-get
            (cdr (assoc (car (split-string (car p) sp)) project-roots))
            key)
           (plist-get (cdr (assoc (car p) project-roots-cache)) key)))
    (when (and (eq :filename-regex key)
               (null val))
      (setq val project-root-file-regexp))
    (when (eq :exclude-paths key)
      (mapc
       (lambda (path)
         (add-to-list 'val path))
       project-root-rep-paths))
    val))

(defun project-root-set-data (prop val &optional p)
  "Set PROP of P to VAL. P is a project, VAL is a property symbol, val is
anything."
  (let ((p (or p (project-root-fetch))))
    (when p
      (unless (assoc (car p) project-roots-cache)
        (add-to-list 'project-roots-cache `(,(car p))))
      (put-alist
       (car p)
       (plist-put (cdr (assoc (car p) project-roots-cache)) prop val)
       project-roots-cache))))

(defun project-root-project-name-from-dir (project)
  "Generate cute name for project from its directory name."
  (upcase-initials (car (last (split-string (cdr project) "/" t)))))

(defun project-root-save-roots ()
  "Saves seen projects info to file. Note that
 this is not done automatically"
  (interactive)
  (with-temp-buffer
    (print project-root-seen-projects (current-buffer))
    (write-file project-root-storage-file)))

(defun project-root-load-roots ()
  "Loads seen projects info from file"
  (interactive)
  (if (file-exists-p project-root-storage-file)
      (with-temp-buffer
        (insert-file-contents project-root-storage-file)
        (setq project-root-seen-projects (read (buffer-string))))))

(defun project-root-fetch (&optional dont-run-on-hit)
  "Attempt to fetch the root project for the current file.

Tests will be used as defined in `project-roots'. Returns fetched project."
  (interactive)
  (let* ((sp project-root-name-split)
         (project
          (catch 'root-found
            (unless (mapc
                     (lambda (project)
                       (let ((name (car project))
                             (root (project-root-get-root (cdr project))))
                         (when root
                           (throw 'root-found (cons (concat name sp root) root)))))
                     project-roots)
              nil))))
    ;; set the actual var used by apps and add to the global project
    ;; list
    (when project
      (project-root-set-project project))))

(defun project-root-set-project (p)
  "Save seen projects to `project-root-storage-file' and set buffer local
`project-details'."
  (unless project-root-seen-projects
      (project-root-load-roots))
  (unless (member p project-root-seen-projects)
    (add-to-list 'project-root-seen-projects p)
    (project-root-save-roots))
  ;; don't set project-details duplicately.
  (unless project-details
    (setq project-details p))
  p)

(defun project-root-every (pred seq)
  "Return non-nil if pred of each element, of seq is non-nil."
  (catch 'got-nil
    (mapc (lambda (x)
            (unless (funcall pred x)
              (throw 'got-nil nil)))
          seq)))

(defun project-root-upward-find-files (filenames &optional startdir)
  "Return the first directory upwards from STARTDIR that contains
all elements of FILENAMES. If STATDIR is nil then use
current-directory."
  (let ((default-directory (expand-file-name (or startdir ".")))
        (depth 0))
    (catch 'pr-finish
      (while t
        ;; don't go too far down the tree
        (when (> (setq depth (1+ depth)) project-root-max-search-depth)
          (throw 'pr-finish nil))
        (cond
          ((project-root-every 'file-exists-p filenames)
           (throw 'pr-finish default-directory))
          ;; if we hit root
          ((string= (expand-file-name default-directory) "/")
           (throw 'pr-finish nil)))
        ;; try again up a directory
        (setq default-directory
              (expand-file-name ".." default-directory))))))

(defun project-root-p (&optional p)
  "Check to see if P or `project-details' is valid"
  (let ((p (or p project-details)))
    (and p (file-exists-p (cdr p)))))

(defmacro with-project-root (&rest body)
  "Run BODY with default-directory set to the project root. Error
if not found. If `project-root' isn't defined then try and find
one."
  (declare (indent 2))
  `(progn
     (unless project-details (project-root-fetch))
     (if (project-root-p)
         (let ((default-directory (cdr project-details))
               (filename-regex (or (project-root-data :filename-regex) ".*"))
               (exclude-paths (project-root-exclude-paths project-details)))
           ,@body)
       (error "No project root found"))))

(defun project-root-files (&optional p file-name-fn)
  "Return an alist of all filenames in the project P. The `car' of each
element in the alist is a simplified file name of full file name, the `cdr' is
the full file name. If FILE-NAME-FN is symbol \"identity\",
`project-root-identity-filename' is used to generate an identity file name for
each project file. Otherwise the relative file name to the project root is
used as the simplified file name."
  (let ((p (or p (project-root-fetch)))
        (file-name-fn (if file-name-fn
                          (cond ((eq file-name-fn 'relative)
                                 'file-relative-name)
                                ((eq file-name-fn 'identity)
                                 'project-root-identity-filename)
                                (t
                                 'file-relative-name))
                        'file-relative-name)))
    (mapcar (lambda (file)
              (cons (funcall file-name-fn file (cdr p)) file))
            (split-string (shell-command-to-string
                           (project-root-find-cmd))))))

(defun project-root-identity-filename (file dir)
  "Generate an identity file name for a project file. FILE is a project file,
DIR is not used."
  (let ((name (replace-regexp-in-string default-directory ""
                                        (expand-file-name file))))
    (mapconcat 'identity (reverse (split-string name "/")) "\\")))

(setq .project-root-find-executable nil)
(defun project-root-find-executable ()
  (if .project-root-find-executable
      .project-root-find-executable
    (setq .project-root-find-executable (executable-find "gfind"))
      (if (not .project-root-find-executable)
          (setq .project-root-find-executable (executable-find "find")))
      .project-root-find-executable))

(defun project-root-exclude-paths (&optional p)
  "Absolute paths of exclude paths."
  (let* ((p (or p (project-root-fetch)))
         (exclude-paths (project-root-data :exclude-paths p)))
    (mapcar
     (lambda (exclude-path)
       (expand-file-name exclude-path (cdr p)))
     exclude-paths)))

(defun project-root-find-cmd (&optional pattern dir p)
  (let* ((p (or p (project-root-fetch)))
         (dir (or dir (cdr p))))
    (concat (project-root-find-executable) " " dir " "
            (project-root-find-prune
             (project-root-exclude-paths p)
             t)
            ", -type f -regex \"" (project-root-data :filename-regex p) "\" "
            (when pattern (concat " -name \"" pattern "\" "))
            project-root-find-options)))

(defun project-root-under-exclude-path (filename &optional p)
  "Determine whether FILENAME is under exclude paths of a project. FILENAME
should be full path of a file."
  (let ((under-exclude-path nil)
        (p (or p (project-root-fetch))))
       (catch 'under-exclude-path
         (mapc
          (lambda (path)
            (when (eq 0 (string-match
                         (regexp-quote (expand-file-name path))
                         filename))
              (setq under-exclude-path t)
              (throw 'under-exclude-path t)))
          (project-root-exclude-paths p)))
       under-exclude-path))

(defun project-root-file-in-project (filename &optional p)
  "Check to see if FILENAME is in the project P. If P is omitted
then the current project-details are used."
  (let ((p (or p (progn
                   (project-root-fetch)
                   project-details))))
    (and
     p
     filename
     (file-exists-p filename)
     (not (null (string-match
                 (regexp-quote (abbreviate-file-name (cdr p)))
                  (abbreviate-file-name filename)))))))

(defun project-root-file-is-project-file (filename &optional p)
  "Determine whether file is a project file. The difference between this
function and project-root-file-in-project is that this function check
whether a file is in project by checking paths, exclude paths and
filename-regex."
  (let* ((p (or p (project-root-fetch)))
         (default-directory (if p (cdr p) default-directory)))
    (and
     p
     filename
     (file-exists-p filename)
     (string-match
      (or (project-root-data :filename-regex p) ".+") filename)
     (string-match
      (regexp-quote (expand-file-name (cdr p)))
      filename)
     (not (project-root-under-exclude-path filename p)))))

(provide 'project-root)

;;; project-root.el ends here
