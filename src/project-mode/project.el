;;; project.el --- minor mode for handling projects
;;
;; Copyright (C) 2015  Qingming He
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
;; + Commentary

(require 'project-root)

;;; project configure/initialize

(defvar prj/use-helm-if-possible t
  "Use helm for completing if possible.")

(defvar prj/helm-mini-include-commands t
  "Whether include source of some useful commands in `prj/helm-mini'.")

(defvar prj/helm-candidate-number-limit 100
  "Limit of helm candidates. This variable should be set before load this
library.")

(defvar prj/use-gtags (if (executable-find "gtags") t nil)
  "Use gtags or ctags?")
(make-variable-buffer-local 'prj/use-gtags)

(defvar prj/tags-tool-found
  (or (executable-find "gtags") (executable-find "ctags"))
  "Whether find a tags generation tool. A tool may be GNU global or Ctags.")

(defvar prj/global-exec (executable-find "global")
  "Executable of GNU global.")

(defvar prj/gtags-conf-file-guess
  `(,(expand-file-name "~/.globalrc")
    "/usr/share/gtags/gtags.conf"
    "/usr/local/share/gtags/gtags.conf"
    "/usr/local/gtags/share/gtags.conf"
    "/opt/share/gtags/gtags.conf"
    "/opt/gtags/share/gtags.conf"
    "/etc/gtags.conf")
  "Guesses of gtags configuration files.")

(defvar prj/gtags-conf-file nil
  "Gtags configuration file.")

(defvar prj/gtags-label-choices '("default" "ctags" "pygments" "native")
  "Gtags label. If \"default\" gtags uses its built in parser; if \"ctags\"
use ctags parser; ...")

(defvar prj/etags-tags-file "TAGS"
  "TAGS file name.")

(defvar prj/buffer-tags-file nil
  "Buffer local `tags-file-name'.")
(make-variable-buffer-local 'prj/buffer-tags-file)

(defvar prj/update-tags-verbose t
  "Update tags file verbosely or not?")

(defvar prj/buffer-mode-lighter nil
  "Mode line linghter for current buffer if `project-minor-mode' is on.")
(make-variable-buffer-local 'prj/buffer-mode-lighter)

(defvar prj/buffer-project nil
  "Buffer local `project-details'.")
(make-variable-buffer-local 'prj/buffer-project)

(defun __file__ ()
  "Get the name of current file."
  (cond ((stringp (car-safe current-load-list)) (car current-load-list))
        (load-file-name)
        ((buffer-file-name))
        ((boundp 'bytecomp-filename) bytecomp-filename)
        (t (symbol-file symbol))))

(defvar prj/package-path (file-name-directory (__file__))
  "Absolute path of project mode package.")

(defvar prj/etags-update-script
  (expand-file-name "etags-update.pl" prj/package-path)
  "Perl script to update TAGS file for a given set of files.")

(defun prj/rel-or-abs-path (path p)
  "Get path.

Path begin with \"/\" or \"~\" will be recognized as absolute path and be
expanded.  Otherwise will be recognized as path relative to project root."
  (if (or
       (= ?/ (string-to-char path))
       (= ?~ (string-to-char path)))
      (expand-file-name path)
    (expand-file-name (concat (cdr p) path))))

(defun prj/setup ()
  "Set up project mode generally.

1. Load project root cache from \"~/.emacs.d/.project-roots;\"
2. Add `project-root-save-roots' to `kill-emacs-hook';
3. Get gtags conf file from guesses."
  (interactive)
  (message "[Prj] loading project roots form %s ..."
           project-root-storage-file)
  (project-root-load-roots)
  (add-hook 'kill-emacs-hook 'project-root-save-roots)
  (setq prj/tags-tool-found
        (or (executable-find "gtags") (executable-find "ctags")))
  (catch 'gtags-conf
    (mapc
     (lambda (f)
       (when (file-exists-p f)
         (message "[Prj] find gtags configuration file %s" f)
         (setq prj/gtags-conf-file f)
         (throw 'gtags-conf f)))
     prj/gtags-conf-file-guess)))

(defun prj/thing-at-point-no-properties (thing)
  "Get thing at point without properties."
  (let* ((bnd (bounds-of-thing-at-point thing))
         (p0 (car bnd))
         (p1 (cdr bnd)))
    (when (and p0 p1)
      (buffer-substring-no-properties p0 p1))))

;;; project files/buffers

(defun prj/occur (arg)
  "Run multiple occur on selected project files.

If arg is nil, run `helm-multi-swoop', otherwise run `multi-occur'.
 All selected project files will be opened before running `helm-multi-swoop'
or `multi-occur' for that these two functions can only applies to buffers. You
may need to kill buffers after running this function."
  (interactive "P")
  (if (featurep 'helm)
      (with-project-root
          (let (prj-files selected-files buffers)
            (setq prj-files (project-root-files))
            (setq selected-files (helm-comp-read
                                  "Select files to run occur: "
                                  (mapcar 'car prj-files)
                                  :marked-candidates t))
            (setq buffers (mapcar
                           (lambda (f)
                             (find-file-noselect
                              (cdr (assoc f prj-files))))
                           selected-files))
            (if (and
                 (featurep 'helm-swoop)
                 prj/use-helm-if-possible
                 (not arg))
                (helm-multi-swoop nil (mapcar 'buffer-name buffers))
              (multi-occur
               buffers
               (read-string
                (format "List lines matching (default \"%s\"): " (symbol-at-point))
                nil nil (symbol-at-point))))))
    (message "Helm is required!")))

(defun prj/occur-dir (arg)
  "Run multiple occur on found files under given directory.

`grep-find-ignored-directories' and `grep-find-ignored-files' will be ignored
by default.  If arg is nil, run `helm-multi-swoop', otherwise run
`multi-occur'. All found files will be opened before running
`helm-multi-swoop' or `multi-occur'. You may need to kill buffers after
running this function."
  (interactive "P")
  (let ((default-directory
          (ido-read-directory-name "Dir to run find: " default-directory))
        cycles
        find-regexp
        find-file-cmd
        files
        buffers
        str)
    (while (not (string-empty-p (setq str (read-string "Dir to ignore: "))))
      (add-to-list 'cycles str))
    (while (not (string-empty-p (setq str (read-string "File name regexp: "))))
      (add-to-list 'find-regexp str))
    (unless find-regexp
      (setq find-regexp '("*")))
    (setq find-file-cmd
          (find-cmd `(prune (name ,@grep-find-ignored-directories ,@cycles))
                    `(not (name ,@grep-find-ignored-files))
                    `(name ,@find-regexp)
                    '(type "f")))
    (setq files
          (split-string (shell-command-to-string find-file-cmd) "\n"))
    (when (and (featurep 'helm) prj/use-helm-if-possible)
      (setq files
            (helm-comp-read
             "Select files to run occur. [M-a] to mark all: "
             files
             :marked-candidates t)))
    (setq buffers
          (mapcar 'find-file-noselect files))
    (if (and
         (featurep 'helm-swoop)
         prj/use-helm-if-possible
         (not arg))
        (helm-multi-swoop nil (mapcar 'buffer-name buffers))
      (multi-occur
       buffers
       (read-string
        (format "List lines matching (default \"%s\"): " (symbol-at-point))
        nil nil (symbol-at-point))))))

(defun prj/find-file ()
  "Find a project file by `helm-comp-read' or `ido-completing-read'."
  (interactive)
  (with-project-root
      (let* ((project-files (project-root-files))
             (file (if (and (featurep 'helm) prj/use-helm-if-possible)
                       (helm-comp-read "Find file in project: "
                                       (mapcar 'car project-files))
                     (ido-completing-read "Find file in project: "
                                          (mapcar 'car project-files)))))
        (find-file (cdr (assoc file project-files))))))

(defun prj/grep ()
  "Run grep at selected project files."
  (interactive)
  (with-project-root
      (let (files
            selected-files
            grep-regexp)
        (setq files (project-root-files))
        (if (and (featurep 'helm) prj/use-helm-if-possible)
            (setq selected-files
                  (helm-comp-read "Files: "
                                  (mapcar 'car files)
                                  :marked-candidates t))
          (setq selected-files (mapcar 'car files)))
        (setq grep-regexp
              (read-string
               (format "regexp (default \"%s\"): " (symbol-at-point))
               nil nil (symbol-at-point)))
        (grep (format "grep -nH -e %s %s"
                grep-regexp
                (mapconcat 'concat
                           (mapcar
                            (lambda (f)
                              (cdr (assoc f files)))
                            selected-files)
                           " "))))))

(defun prj/find-grep-dir ()
  "Run grep with find under certain directory.

The difference between this function and `prj/grep' is that `prj/grep' only
applies to selected project files and this function applies to any found files
under certain directory. This function need not run at project root."
  (interactive)
  (let ((default-directory
          (ido-read-directory-name "Dir to run find: " default-directory))
        (grep-regexp
         (read-string
          (format "grep regexp (default \"%s\"): " (symbol-at-point))
          nil nil (symbol-at-point)))
        find-file-cmd
        cycles
        find-regexp
        str)
    (while (not (string-empty-p (setq str (read-string "Dir to ignore: "))))
      (add-to-list 'cycles str))
    (while (not (string-empty-p (setq str (read-string "File name regexp: "))))
      (add-to-list 'find-regexp str))
    (unless find-regexp
      (setq find-regexp '("*")))
    (setq find-file-cmd
          (find-cmd
           `(prune (name ,@grep-find-ignored-directories ,@cycles))
           `(not (name ,@grep-find-ignored-files))
           `(name ,@find-regexp)
           '(type "f")))
    (find-grep
     (format "%s -exec grep -nH -e %s {} +"
             find-file-cmd
             grep-regexp))))

(defun prj/find-dired ()
  "Run find and go into Dired mode on a buffer of the output."
  (interactive)
  (let ((dir
         (ido-read-directory-name "Dir to run find: " default-directory))
        (find-cmd-prune
         (when (y-or-n-p "Ignore grep-find-ignored-directories?")
           grep-find-ignored-directories))
        (find-cmd-not
         (when (y-or-n-p "Ignore grep-find-ignored-files?")
           grep-find-ignored-files))
        find-cmd-name
        str)
    (while (not (string-empty-p (setq str (read-string "Dir to ignore: "))))
      (add-to-list 'find-cmd-prune str))
    (when find-cmd-prune
      (setq find-cmd-prune `(prune (name ,@find-cmd-prune))))
    (while (not (string-empty-p (setq str (read-string "files to ignore: "))))
      (add-to-list 'find-cmd-not str))
    (when find-cmd-not
      (setq find-cmd-not `(not (name ,@find-cmd-not))))
    (while (not (string-empty-p (setq str (read-string "file name regexp: "))))
      (add-to-list 'find-cmd-name str))
    (when find-cmd-name
      (setq find-cmd-name `(name ,@find-cmd-name)))
    (find-dired
     dir
     (mapconcat
      'concat
      (nthcdr
       2
       (split-string
        (find-cmd find-cmd-prune find-cmd-not find-cmd-name)))
      " "))))

(defun prj/goto-project (&optional p)
  "Go to root of a selected project in Dired."
  (interactive)
  (let ((path (if p
                  (cdr p)
                (cdr (assoc
                      (ido-completing-read
                       "Project: "
                       (mapcar 'car project-root-seen-projects))
                      project-root-seen-projects)))))
    (find-file path)))

(defun prj/kill-buffers-dir ()
  "Kill all buffers under directory, Dired buffers included. The buffers are
saved before killed."
  (interactive)
  (let ((dir (ido-read-directory-name "Directory: ")))
    (mapcar
     (lambda (buffer)
       (progn (set-buffer buffer)
              (when (equal
                     0
                     (string-match-p
                      dir (expand-file-name default-directory)))
                (when (and
                       (buffer-file-name)
                       (buffer-modified-p))
                  (save-buffer))
                (message (format "Kill %s" (or (buffer-file-name) (buffer-name))))
                (kill-buffer buffer))))
     (buffer-list))))

(defun prj/kill-project-buffers (&optional p)
  "Kill all opened buffers of project. The buffers are saved before killed."
  (interactive)
  (let* ((default-directory
           (if p
               (cdr p)
             (cdr (assoc
                   (ido-completing-read
                    "Project: "
                    (mapcar 'car project-root-seen-projects))
                   project-root-seen-projects))))
         (project-files (split-string
                         (shell-command-to-string
                          (project-root-find-cmd)))))
    (mapcar
     (lambda (buffer)
       (progn (set-buffer buffer)
              (let ((fname (buffer-file-name)))
                (when (and
                       fname
                       (member fname project-files))
                  (when (buffer-modified-p)
                    (save-buffer))
                  (message (format "Kill %s" (buffer-file-name)))
                  (kill-buffer buffer)))))
     (buffer-list))))

;;; project tags

(defun prj/generate-etags ()
  "Make $PROJECT-TAGS at project root. You'd better generate tags file at
project root, otherwise prj can't update tags file."
  (interactive)
  (let* ((p (or prj/buffer-project (project-root-fetch)))
         (default-directory (cdr p))
         (tags-file prj/etags-tags-file))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (message "Generating TAGS file for %s ..." (car p))
    (shell-command
     (format "%s | xargs ctags -e -f %s -a"
             (project-root-find-cmd) tags-file))
    (message "Done")))

(defun prj/get-gtags-label (&optional p)
  "Get gtags label."
  (let ((p (or p (project-root-fetch))))
    (or (getenv "GTAGSLABEL")
        (project-root-data :gtags-label p)
        (ido-completing-read
         "Gtags label (You'd better set :gtags-label in project-roots): "
         prj/gtags-label-choices))))

(defun prj/get-gtags-conf (&optional p)
  "Get gtags conf."
  (let ((p (or p (project-root-fetch))))
    (or (getenv "GTAGSCONF")
        (project-root-data :gtags-conf p)
        prj/gtags-conf-file
        (ido-read-file-name "Gtags configuration file: "))))

(defun prj/generate-gtags ()
  "Run gtags at user specified path."
  (interactive)
  (let* ((p (or prj/buffer-project (project-root-fetch)))
         (default-directory (cdr p))
         (gtags-label (prj/get-gtags-label p))
         (gtags-conf (prj/get-gtags-conf p)))
    (message "Generating GTAGS for %s ..." (car p))
    (shell-command
     (format "%s | gtags --gtagslabel=%s --gtagsconf=%s --file=-"
             (project-root-find-cmd) gtags-label gtags-conf))
    (message "Done")))

(defun prj/generate-tags ()
  "Generate tags file by ctags or gtags."
  (interactive)
  (if prj/use-gtags
      (prj/generate-gtags)
    (prj/generate-etags)))

(defun prj/update-gtags-single-file (&optional p)
  "Update GTAGS for current project file."
  (let ((fname (buffer-file-name))
        (p (or p (project-root-fetch))))
    (when (and p fname)
      (let ((gtags-label (prj/get-gtags-label))
            (gtags-conf (prj/get-gtags-conf)))
        (shell-command
         (format
          "%s --gtagslabel=%s --gtagsconf=%s --single-update %s"
          prj/global-exec gtags-label gtags-conf fname))))))

(defun prj/update-etags-single-file (&optional p)
  "Update TAGS for current project file."
  (let ((fname (buffer-file-name))
        (p (or p (project-root-fetch))))
    (when (and p fname prj/buffer-tags-file)
      (shell-command
       (format "%s %s %s"
               prj/etags-update-script prj/buffer-tags-file fname)))))

(defun prj/update-tags-single-file ()
  "Update tags for single file by ctags or gtags."
  (when (and
         (buffer-file-name))
    (when prj/update-tags-verbose
      (message "Updating tags for %s ..." (buffer-file-name)))
    (if prj/use-gtags
        (prj/update-gtags-single-file prj/buffer-project)
      (prj/update-etags-single-file prj/buffer-project))
    (when prj/update-tags-verbose
      (message "Done"))))

(defun prj/save-buffers-and-update-tags-sentinel (proc event)
  (when (eq (process-status proc) 'exit)
    (if (zerop (process-exit-status proc))
        (message "Updating tags for %s Done." proc)
      (message "Updating tags for %s failed." proc))))

(defun prj/save-buffers-and-update-tags ()
  "Update tags file for modified buffers of projects asynchronously then save
all modified buffers."
  (interactive)
  (let ((prj-files (make-hash-table :test 'equal))
        not-prj-buffers p f proc)
    ;; get all modified buffers or files and classify them according to which
    ;; project they belong to
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (when (and
                (setq f (buffer-file-name))
                (buffer-modified-p))
           (if (and
                (setq p prj/buffer-project)
                (not (string=
                      "none"
                      (project-root-data :tags-tool prj/buffer-project))))
               (progn
                 (puthash (car p) (cons f (gethash (car p) prj-files)) prj-files)
                 (remove-hook 'after-save-hook 'prj/update-tags-single-file t))
             (add-to-list 'not-prj-buffers buf)))))
     (buffer-list))
    ;; save project buffers and update tags file using GNU global or
    ;; etags-update.pl project-wisely
    (maphash
     (lambda (key value)
       ;; save buffers and re-add the `prj/update-tags-single-file' to
       ;; `after-save-hook'
       (mapcar
        (lambda (f)
          (with-current-buffer (get-file-buffer f)
            (save-buffer)
            (add-hook 'after-save-hook 'prj/update-tags-single-file nil t)))
        value)
       ;; update tags
       (when prj/update-tags-verbose
         (message "Updating tags file for %s ..." key))
       (setq p (assoc key project-root-seen-projects))
       (with-current-buffer (get-file-buffer (car value))
         (if prj/use-gtags
             (if (= 1 (length value))
                 (setq proc
                       (start-process
                        key nil prj/global-exec
                        (format "--gtagslabel=%s" (prj/get-gtags-label p))
                        (format "--gtagsconf=%s" (prj/get-gtags-conf p))
                        (format "--single-update=%s" (car value))))
               (setq proc
                     (start-process
                      key nil prj/global-exec "-u"
                      (format "--gtagslabel=%s" (prj/get-gtags-label p))
                      (format "--gtagsconf=%s" (prj/get-gtags-conf p)))))
           (setq proc
                 (eval
                  `(start-process key nil prj/etags-update-script
                                  prj/buffer-tags-file ,@value))))
         (when prj/update-tags-verbose
           (set-process-sentinel
            proc
            'prj/save-buffers-and-update-tags-sentinel))))
     prj-files)
    ;; save non project buffers
    (mapcar
     (lambda (buf) (with-current-buffer buf (save-buffer)))
     not-prj-buffers)))

;;; project compiler flags

(defun prj/c-include-paths-pkgs (pkgs)
  "Get c include pahts for \"pkgs\". \"pkgs\" should be string or list of
  string.

Valid form of \"pkgs\":
\"glib\", \"glib python-2.7\", '(\"glib\" \"python-2.7\")

Returns:
List of include paths of \"pkgs\", including \"-I\" flag."
  (when (and pkgs (executable-find "pkg-config"))
    (let ((c-include-paths nil)
          (pkgs (cond ((listp pkgs) pkgs)
                      ((stringp pkgs) (split-string pkgs)))))
      (mapc
       (lambda (pkg)
         (mapc
          (lambda (inc)
            (when (and
                   (> (length inc) 2)
                   (string= "-I" (substring inc 0 2)))
              (add-to-list 'c-include-paths (substring inc 2))))
          (split-string (shell-command-to-string
                         (format "pkg-config --cflags-only-I %s" pkg)))))
       pkgs)
      c-include-paths)))

(defun prj/c-include-paths-general (language)
  "Get general C or C++ include paths.

LANGUAGE: \"c\" or \"c++\".

Returns:
List of include paths, include \"-I\" flag."
  (let (p1
        p2
        c-include-paths
        (compiler
         (cond ((string= "c" language) "gcc")
               ((string= "c++" language) "g++")
               (t (error (format "%s not supported!" language))))))
    (when (executable-find compiler)
      (with-temp-buffer
        (insert (shell-command-to-string
                 (format "echo \"\" | %s -v -x %s -E -" compiler language)))
        (goto-char (point-min))
        (search-forward "#include <...>")
        (forward-line 1)
        (setq p1 (line-beginning-position))
        (search-forward "# 1")
        (forward-line -2)
        (setq p2 (line-end-position))
        (setq c-include-paths
              (split-string (buffer-substring-no-properties p1 p2)))
        (add-to-list 'c-include-paths ".")))
    (mapcar
     (lambda (path)
       (concat "-I" path))
     c-include-paths)))

;;; project helm mini

(defun prj/helm-buffers-candidates ()
  (let (buffers
        (p (or prj/buffer-project (project-root-fetch))))
    (when p
      (mapc
       (lambda (buffer)
         (with-current-buffer buffer
           (when (project-root-file-is-project-file (buffer-file-name) p)
             (add-to-list 'buffers (cons (buffer-name) buffer)))))
       (buffer-list)))
    buffers))

(defun prj/helm-save-buffers (buffer)
  (mapc
   (lambda (b)
     (with-current-buffer b
       (when (and
              (not buffer-read-only)
              (buffer-file-name)
              (buffer-modified-p))
         (save-buffer))))
   (helm-marked-candidates)))

(defun prj/helm-kill-buffers (buffer)
  (mapc
   (lambda (b)
     (with-current-buffer b
       (when (and
              (not buffer-read-only)
              (buffer-file-name)
              (buffer-modified-p))
         (save-buffer))
       (kill-buffer)))
   (helm-marked-candidates)))

(defun prj/helm-multi-occur (buffer)
  (multi-occur
   (helm-marked-candidates)
   (read-string
    (format "pattern (default \"%s\"): "
            (prj/thing-at-point-no-properties 'symbol))
    nil nil (prj/thing-at-point-no-properties 'symbol))))

(defun prj/helm-multi-swoop (buffer)
  (helm-multi-swoop
   nil
   (mapcar 'buffer-name (helm-marked-candidates))))

(defun prj/helm-find-files (file)
  (if (eq 1 (length (helm-marked-candidates)))
      (find-file file)
    (mapc
     (lambda (f)
       (find-file-noselect f))
     (helm-marked-candidates))))

(defun prj/helm-grep-files (file)
  (grep (format "grep -nH -e %s %s"
                (read-string (format "pattern (default \"%s\"): "
                                     (prj/thing-at-point-no-properties 'symbol))
                             nil nil (prj/thing-at-point-no-properties 'symbol))
                (mapconcat 'concat
                           (helm-marked-candidates)
                           " "))))

(defun prj/helm-gen-tags (proot)
  (let ((default-directory proot))
    (if (y-or-n-p "Use GNU global? ")
        (shell-command
         (format "%s | gtags --gtagslabel=%s --gtagsconf=%s --file=-"
                 (project-root-find-cmd)
                 (prj/get-gtags-label)
                 (prj/get-gtags-conf)))
      (let ((tags-file prj/etags-tags-file))
        (when (file-exists-p tags-file)
          (delete-file tags-file))
        (shell-command
         (format "%s | xargs ctags -e -f %s -a"
                 (project-root-find-cmd) tags-file))))))

(defun prj/helm-create-new-file (file)
  (let* ((p (or prj/buffer-project (project-root-fetch)))
         (default-directory (cdr p)))
    (let ((default-directory
            (ido-read-directory-name "Directory: ")))
      (when (not (file-exists-p default-directory))
        (mkdir default-directory))
      (find-file (read-string "File name: ")))))

(defun prj/helm-files-candidates ()
  (let ((p (or prj/buffer-project (project-root-fetch))))
    (when p
      (let ((default-directory (cdr p)))
        (project-root-files)))))

(defun prj/helm-seen-projects ()
  project-root-seen-projects)

(defun prj/helm-remove-seen-projects (pr)
  (mapc
   (lambda (pr)
     (setq project-root-seen-projects
           (rassq-delete-all pr project-root-seen-projects)))
   (helm-marked-candidates))
  (project-root-save-roots))

(defun prj/helm-jump-to-dired-buffer (buffer)
  (with-current-buffer buffer
    (dired-jump)))

(defun prj/helm-jump-to-dired-file (file)
  (dired-jump nil file))

(defun prj/helm-mini ()
  (interactive)
  (require 'helm)
  (helm :sources `(((name . "Project Buffers")
                    (candidates . prj/helm-buffers-candidates)
                    (candidate-number-limit . ,prj/helm-candidate-number-limit)
                    (action . (("Switch to buffer" . switch-to-buffer)
                               ("Switch to buffer other window" . switch-to-buffer-other-window)
                               ("Jump to Dired" . prj/helm-jump-to-dired-buffer)
                               ("Save buffer(s)" . prj/helm-save-buffers)
                               ("Kill buffer(s)" . prj/helm-kill-buffers)
                               ("Multi occur on buffer(s)" . prj/helm-multi-occur)
                               ("Helm multi swoop on buffer(s)" . prj/helm-multi-swoop))))
                   ((name . "Project Files")
                    (candidates . prj/helm-files-candidates)
                    (candidate-number-limit . ,prj/helm-candidate-number-limit)
                    (action . (("Find file(s)" . prj/helm-find-files)
                               ("Find file other window" . find-file-other-window)
                               ("Jump to Dired" . prj/helm-jump-to-dired-file)
                               ("Grep files" . prj/helm-grep-files))))
                   ((name . "Create New File")
                    (dummy)
                    (action . prj/helm-create-new-file))
                   ((name . "Seen Projects")
                    (candidates . prj/helm-seen-projects)
                    (action . (("Find project root in Dired" . find-file)
                               ("Generate TAGS" . prj/helm-gen-tags)
                               ("Remove from seen projects" . prj/helm-remove-seen-projects)))))
        :buffer "*project helm mini*"))

;;; project minor/global mode

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  :lighter prj/buffer-mode-lighter
  (if project-minor-mode
      (progn
        (let ((p (or prj/buffer-project
                     (setq prj/buffer-project (project-root-fetch))))
              (fname (buffer-file-name))
              lght
              tags-tool)
          (when (and
                 p
                 fname
                 (file-exists-p fname)
                 (not (file-remote-p fname)))
            ;; mode line lighter
            (setq
             prj/buffer-mode-lighter
             (if (setq lght (project-root-data :lighter p))
                 (format " Prj:%s" lght)
               " Prj"))
            ;; add update tags hook, determine tags tool, generate tags
            (unless (or
                     (string=
                      "none"
                      (setq tags-tool (project-root-data :tags-tool p)))
                     (not prj/tags-tool-found))
              (when (project-root-file-is-project-file fname p)
                (add-hook 'after-save-hook
                          'prj/update-tags-single-file nil t))
              (cond ((string= "gtags" tags-tool) (setq prj/use-gtags t))
                    ((string= "ctags" tags-tool) (setq prj/use-gtags nil))
                    (t (if (string-match ".+gtags$" prj/tags-tool-found)
                           (setq prj/use-gtags t)
                         (setq prj/use-gtags nil))))
              (let ((default-directory (cdr p)))
                (if prj/use-gtags
                    (unless (file-exists-p "GTAGS")
                      (prj/generate-gtags))
                  (unless (file-exists-p prj/etags-tags-file)
                    (prj/generate-etags))
                  (setq prj/buffer-tags-file
                        (expand-file-name prj/etags-tags-file))
                  (setq-local tags-file-name
                              (expand-file-name prj/etags-tags-file))))))))
    (remove-hook 'after-save-hook 'prj/update-tags-single-file t)))

(defun project-mode-on-safe ()
  "Enable `global-project-mode' if it is safe to do so.

Enable `global-project-mode' only when all following conditions are meet:
1. a project is found;
2. buffer has a file name;
3. file exists;
4. file is a project file."
  (let ((fname (buffer-file-name)))
    (when (and
           fname
           (file-exists-p fname)
           (not (file-remote-p fname))
           (setq prj/buffer-project (project-root-fetch)))
      (project-minor-mode))))

(define-globalized-minor-mode global-project-mode project-minor-mode
  project-mode-on-safe
  :init-value nil
  :require 'project)

(provide 'project)

;;; project.el ends here
