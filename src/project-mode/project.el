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

(require 'project-root)
(require 'etags-update)

(defvar prj/use-helm-if-possible t
  "Use helm for completing if possible.")

(defvar prj/use-gtags (if (executable-find "gtags")
                          t
                        nil)
  "Use gtags or ctags?")

(defvar prj/gtags-conf-file-guess
  `("/usr/share/gtags/gtags.conf"
    "/usr/local/share/gtags/gtags.conf"
    "/usr/local/gtags/share/gtags.conf"
    "/opt/share/gtags/gtags.conf"
    "/opt/gtags/share/gtags.conf"
    ,(expand-file-name "~/.globalrc")
    "/etc/gtags.conf")
  "Guesses of gtags configuration files.")

(defvar prj/gtags-conf-file nil
  "Gtags configuration file.")

(defvar prj/gtags-label-choices '("default" "ctags" "pygments" "native")
  "Gtags label. If \"default\" gtags uses its built in parser; if \"ctags\"
use ctags parser; ...")

(defvar prj/install-path (expand-file-name "~/bin")
  "Where to install scripts of project mode.")

(defun __file__ ()
  "Get the name of current file."
  (cond ((stringp (car-safe current-load-list)) (car current-load-list))
        (load-file-name)
        ((buffer-file-name))
        ((boundp 'bytecomp-filename) bytecomp-filename)
        (t (symbol-file symbol))))

(defvar prj/path (file-name-directory (__file__))
  "Absolute path of project mode package.")

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
3. Get gtags conf file from guesses;
4. Install \"etags-update.pl\" which will be used to update tags file
generated by ctags. Note that \"etags-update.pl\" will be installed to
`prj/install-path'. You may have to add value of `prj/install-path' to
\"$PATH\" environment variable."
  (interactive)
  (message "[Prj] loading project roots form %s ..."
           project-root-storage-file)
  (project-root-load-roots)
  (add-hook 'kill-emacs-hook 'project-root-save-roots)
  (catch 'gtags-conf
    (mapc
     (lambda (f)
       (when (file-exists-p f)
         (message "[Prj] find gtags configuration file %s" f)
         (setq prj/gtags-conf-file f)
         (throw 'gtags-conf f)))
     prj/gtags-conf-file-guess))
  (unless (executable-find "etags-update.pl")
    (message "[Prj] installing etags-update.pl ...")
    (unless (file-exists-p prj/install-path)
      (make-directory prj/install-path))
    (copy-file (format "%s/etags-update.pl" prj/path)
               prj/install-path
               t)
    (chmod (format "%s/etags-update.pl" prj/install-path) 755)))

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
  "Run multiple occur on found files under given directory..

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

(defun prj/generate-etags ()
  "Make $PROJECT-TAGS at project root. You'd better generate tags file at
project root, otherwise prj can't update tags file."
  (interactive)
  (let* ((p (project-root-fetch))
         (default-directory
           (ido-read-directory-name
            "Directory to run ctags: "
            (when p (cdr p))))
         (tags-file
          (read-string
           "Name of tags file: " nil nil
           (concat (if p (concar (car p) "-") "") "TAGS"))))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (shell-command
     (format "%s | xargs ctags -e -f %s -a"
             (project-root-find-cmd) tags-file))))

(defun prj/get-gtags-label ()
  "Get gtags label."
  (or (getenv "GTAGSLABEL")
      (project-root-data :gtags-label p)
      (ido-completing-read
       "Gtags label (You'd better set :gtags-label in project-roots): "
       prj/gtags-label-choices)))

(defun prj/get-gtags-conf ()
  "Get gtags conf."
  (or (getenv "GTAGSCONF")
      (project-root-data :gtags-conf p)
      prj/gtags-conf-file
      (ido-read-file-name "Gtags configuration file: ")))

(defun prj/generate-gtags ()
  "Run gtags at user specified path."
  (interactive)
  (let* ((p (project-root-fetch))
         (default-directory (ido-read-directory-name
                             "Directory to run gtags: "
                             (when p (cdr p))))
         (gtags-label (prj/get-gtags-label))
         (gtags-conf (prj/get-gtags-conf)))
    (shell-command
     (format "%s | gtags --gtagslabel=%s --gtagsconf=%s --file=-"
             (project-root-find-cmd) gtags-label gtags-conf))))

(defun prj/generate-tags ()
  "Generate tags file by ctags or gtags."
  (interactive)
  (if prj/use-gtags
      (prj/generate-gtags)
    (prj/generate-gtags)))

(defun prj/update-gtags-single-file ()
  "Update GTAGS for current project file."
  (let ((fname (buffer-file-name))
        (p (project-root-fetch)))
    (when (and p fname)
      (let ((gtags-label (prj/get-gtags-label))
            (gtags-conf (prj/get-gtags-conf)))
        (shell-command
         (format
          "global --gtagslabel=%s --gtagsconf=%s --single-update %s"
          gtags-label gtags-conf fname))))))

(defun prj/update-tags-single-file ()
  "Update tags for single file by ctags or gtags."
  (if prj/use-gtags
      (prj/update-gtags-single-file)
    (etu/update-tags-for-file)))

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

(defun prj/set-gfortran-compile-args (&optional p)
  "Set flycheck-gfortran-* variables."
  (when (and
         (equal 'f90-mode major-mode)
         (featurep 'flycheck))
      (let ((p (or p (project-root-fetch))))
        ;; gfortran include paths
        (mapc
         (lambda (include-path)
           (add-to-list 'flycheck-gfortran-include-path
                        (prj/rel-or-abs-path include-path p)))
         (or (project-root-data :gfortran-include-paths p) '(".")))
        ;; gfortran pre-definitions
        (when (project-root-data :gfortran-definitions p)
          (setq flycheck-gfortran-definitions
                (project-root-data :gfortran-definitions p)))
        ;; gfortran language standard
        (when (project-root-data :gfortran-language-standard p)
          (setq flycheck-gfortran-language-standard
                (or (project-root-data :gfortran-language-standard p) "f2008")))
        ;; whether use hdf5 library
        (when (project-root-data :use-hdf5 p)
          (let ((hdf5-root (getenv "HDF5_ROOT")))
            (when hdf5-root
              (add-to-list 'flycheck-gfortran-include-path
                           (concat hdf5-root "/include")))))
        ;; where to generate module files
        (when (project-root-data :gfortran-J p)
          (setq flycheck-gfortran-J-path
                (prj/rel-or-abs-path (project-root-data :gfortran-J p) p))))))

(defun prj/c-include-paths-pkgs (pkgs)
  "Get c include pahts for \"pkgs\". \"pkgs\" should be string or list of
  string.

Valid form of \"pkgs\":
\"glib\", \"glib python-2.7\", '(\"glib\" \"python-2.7\")

Returns:
Include paths of \"pkgs\", including \"-I\" flag."
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

(defun prj/set-c-args (&optional p)
  "Set flycheck-*-include-path, ac-clang-flags, cc-search-directories."
  )

(defun prj/set-compile-args (&optional p)
  "Set flycheck-checker-* variables and ac-clang-flags."
  (let ((p (or p (project-root-fetch))))
    (when p
      (cond ((equal 'f90-mode major-mode)
             (prj/set-gfortran-compile-args p))
            ((equal 'c-mode major-mode)
             nil)))))

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  :lighter (:eval (let ((p (project-root-fetch))
                        (fname (buffer-file-name))
                        lght
                        plght)
                    (when (and
                           p
                           fname
                           (file-exists-p fname)
                           (project-root-file-is-project-file fname p))
                      (setq lght " Prj")
                      (setq plght (project-root-data :lighter p))
                      (when plght
                        (setq lght (format "%s:%s" lght plght))))
                    lght))
  (if project-minor-mode
      (progn
        (let ((p (project-root-fetch))
              (fname (buffer-file-name)))
          (when (and
                 p
                 fname
                 (file-exists-p fname)
                 (project-root-file-is-project-file fname p))
            (add-hook 'after-save-hook 'prj/update-tags-single-file nil t)
            (prj/set-compile-args p))))
    (progn
      (remove-hook 'after-save-hook 'prj/update-tags-single-file t))))

(defun project-mode-on-safe ()
  "Enable `global-project-mode' if it is safe to do so.

Enable `global-project-mode' only when all following conditions are meet:
1. a project is found;
2. buffer has a file name;
3. file exists;
4. file is a project file."
  (let ((p (project-root-fetch))
        (fname (buffer-file-name)))
    (when (and
           p
           fname
           (file-exists-p fname)
           (project-root-file-is-project-file fname p))
      (project-minor-mode))))

(define-globalized-minor-mode global-project-mode project-minor-mode
  project-mode-on-safe
  :init-value nil
  :require 'project)

(provide 'project)

;;; project.el ends here
