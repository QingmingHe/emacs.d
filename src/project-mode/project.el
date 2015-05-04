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
;;; Commentary:
;; +. features:
;;    - Handle tags file automatically. Generating, updating, ...
;;    - Provide a preconfigured `helm' to list project buffers, files and seen
;;      projects.
;;    - Two `auto-complete' sources `prj/ac-source-etags' and
;;      `prj/ac-source-gtags' which handles ac-source for each project.
;;    - Some useful command to run find, grep, ... at project files or buffers.

(require 'project-root)
(require 'cl)
(require 'etags)

;;; project configure/initialize

(defvar prj/auto-set-flags-if-needed t
  "Whether auto set flags.")

(defvar prj/temp-dir
  (expand-file-name "project-temp-dir/" temporary-file-directory)
  "Temporary directory.")

(defvar prj/cmake-find-packages-headers
  "cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)
"
  "Header content of CMake script.")

(defvar prj/cmake-find-mpi-content
  "find_package(MPI)
message(STATUS \"[prj] mpi-fortran-include-path: ${MPI_Fortran_INCLUDE_PATH}\")
message(STATUS \"[prj] mpi-c-include-path: ${MPI_C_INCLUDE_PATH}\")
message(STATUS \"[prj] mpi-CXX-include-path: ${MPI_CXX_INCLUDE_PATH}\")
"
  "CMake script content to find MPI.")

(defvar prj/cmake-find-hdf5-content
  "find_package(HDF5)
message(STATUS \"[prj] hdf5-include-dirs: ${HDF5_INCLUDE_DIR}\")
"
  "CMake script content to find HDF5.")

(defvar prj/cmake-exec (executable-find "cmake")
  "Executable for CMake.")

(defvar prj/project-locals-file ".project-locals.el"
  "Files containing project local variables.")

(defvar prj/load-project-locals-if-exists t
  "Whether load project local variables.")

(defvar prj/helm-etags-splitter " \001 "
  "Splitter of file name and line in `prj/helm-etags'.")

(defvar prj/helm-etags-match-part t
  "Whether not match file part of tag.")

(defvar prj/helm-etags-input-at-point t
  "Whether take symbol at point as default input.")

(defvar prj/helm-etags-execute-action-at-once-if-one t
  "Whether jump if only one match.")

(defvar prj/auto-insert-after-find-file t
  "Whether call `auto-insert' after finding project file.")

(defvar prj/touch-cmake-lists-at-create-new-file t
  "Whether touch CMakeLists.txt at creating new project file by
`prj/add-new-source'.")

(defvar prj/cmake-modes '('fortran-mode 'f90-mode 'c-mode 'c++-mode)
  "Modes that CMake supported natively.")

(defvar prj/cmake-list-file "CMakeLists.txt"
  "CMake file.")

(defvar prj/helm-candidate-number-limit 100
  "Limit of helm candidates. This variable should be set before load this
library.")

(defvar prj/tags-tool-found
  (or (executable-find "gtags") (executable-find "ctags"))
  "Whether find a tags generation tool. A tool may be GNU global or Ctags.")

(defvar prj/global-exec (executable-find "global")
  "Executable of GNU global.")

(defvar prj/ctags-exec (executable-find "ctags")
  "Executable of Exuberant Ctags.")

(defvar prj/gtags-conf-file-guess
  `(,(expand-file-name "~/.globalrc")
    "/usr/share/gtags/gtags.conf"
    "/usr/local/share/gtags/gtags.conf"
    "/usr/local/gtags/share/gtags.conf"
    "/opt/share/gtags/gtags.conf"
    "/opt/gtags/share/gtags.conf"
    "/etc/gtags.conf")
  "Guesses of gtags configuration files.")

(defvar prj/gtags-conf-file
  (let (gtags-conf)
    (catch 'gtags-conf
      (mapc
       (lambda (f)
         (when (file-exists-p f)
           (message "[Prj] find gtags configuration file %s" f)
           (setq gtags-conf f)
           (throw 'gtags-conf f)))
       prj/gtags-conf-file-guess))
    gtags-conf)
  "Gtags configuration file.")

(defvar prj/etags-tags-file "TAGS"
  "TAGS file name.")

(defvar prj/update-tags-verbose t
  "Update tags file verbosely or not?")

(defvar prj/buffer-mode-lighter nil
  "Mode line linghter for current buffer if `project-minor-mode' is on.")
(make-variable-buffer-local 'prj/buffer-mode-lighter)

(defvar prj/use-completion nil
  "Whether use `company' or `auto-complete'.")

(defvar prj/completion-backend nil
  "Completion backend of project buffers.")

(defvar prj/use-tags t
  "Whether use ctags or gtags for handling project.")

(defvar prj/ac-etags-source-defined nil
  "Whether ac-source-prj/etags has been defined.")

(defvar prj/ac-gtags-source-defined nil
  "Whether ac-source-prj/gtags has been defined.")

(defface prj/ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface prj/ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defface prj/ac-gtags-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for gtags candidate")

(defface prj/ac-gtags-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the gtags selected candidate.")

(defvar prj/extra-company-backeds '(company-dabbrev-code company-keywords)
  "Extra `company-backeds' grouped with `company-gtags' or `prj/company-etags'.")

(defvar prj/buffer-is-using-etags-backend nil
  "Whether current buffer is using `prj/company-etags'.")
(make-variable-buffer-local 'prj/buffer-is-using-etags-backend)

(with-eval-after-load 'helm
  (defvar prj/helm-etags-map
    (let ((map helm-map))
      (define-key map (kbd "C-c C-t")  'prj/helm-etags-toggle-match-part)
      map)
    "Keymap used in Etags."))

(defun __file__ ()
  "Get the name of current file."
  (cond ((stringp (car-safe current-load-list)) (car current-load-list))
        (load-file-name)
        ((buffer-file-name))
        ((boundp 'bytecomp-filename) bytecomp-filename)
        (t (symbol-file symbol))))

(defvar prj/package-path (file-name-directory (__file__))
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
2. Add `project-root-save-roots' to `kill-emacs-hook';"
  (message "[Prj] loading project roots form %s ..."
           project-root-storage-file)
  (project-root-load-roots)
  (add-hook 'kill-emacs-hook 'project-root-save-roots))

(defun prj/thing-at-point-no-properties (thing)
  "Get thing at point without properties."
  (let* ((bnd (bounds-of-thing-at-point thing))
         (p0 (car bnd))
         (p1 (cdr bnd)))
    (when (and p0 p1)
      (buffer-substring-no-properties p0 p1))))

;;; project files/buffers

(defun prj/find-file ()
  "Find a file from a list of those that exist in the current project."
  (interactive)
  (let ((p (or project-details (project-root-fetch))))
    (if p
        (let* ((files (project-root-files p 'identity))
               (file (ido-completing-read "Find file in project: "
                                          (mapcar 'car files))))
          (find-file (cdr (assoc file project-files)))))))

(defun prj/occur (pattern &optional p)
  "Run multiple occur on project buffers. Whether a file is a project file is
determined by `project-root-file-is-project-file'."
  (interactive
   (list
    (read-string (format "pattern (default \"%s\"): " (symbol-at-point))
                 nil nil (prj/thing-at-point-no-properties 'symbol))))
  (let* ((p (or p (project-root-fetch)))
         (buffers (let (buffers)
                    (mapc
                     (lambda (buf)
                       (with-current-buffer buf
                         (when (project-root-file-is-project-file
                                buffer-file-name p)
                           (setq buffers (cons buf buffers)))))
                     (buffer-list))
                    buffers)))
    (multi-occur buffers pattern)))

(defun prj/grep (grep-regexp wildcard dir &optional p)
  "Run grep with find at project files."
  (interactive
   (list (read-string
          (format "regexp (default \"%s\"): " (symbol-at-point))
          nil nil (prj/thing-at-point-no-properties 'symbol))
         (read-string
          (format "file name wildcard (default \"*.%s\"): "
                  (file-name-extension (buffer-name)))
          nil nil (format "*.%s" (file-name-extension (buffer-name))))
         (ido-read-directory-name "Dir to run grep: ")))
  (let ((p (or p (project-root-fetch))))
    (grep
     (format "%s | xargs grep -nH -e \"%s\""
             (project-root-find-cmd wildcard dir p)
             grep-regexp))))

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

(defun prj/go-up-dir-find-file (file &optional p path-begin)
  "Go up directory to find FILE starts from PATH-BEGIN till root of
project. FILE should be name without directory. If P is not given,
`project-details' is used, if `project-details' is `nil', `project-root-fetch'
is used to obtain P. If PATH-BEGIN is not given, `default-directory' is
used. Returns a list of full file names whose file name is FILE in a sequence
from project root to PATH-BEGIN."
  (let ((p (or p project-details (project-root-fetch)))
        (dir (or path-begin default-directory))
        jump-out
        file-full-path)
    (when p
      (while (not jump-out)
        (when (file-exists-p (expand-file-name file dir))
          (setq file-full-path (cons (expand-file-name file dir) file-full-path)))
        (when (string= (cdr p) dir)
          (setq jump-out t))
        (setq dir (file-name-directory (directory-file-name dir)))))
    file-full-path))

(defun prj/add-new-source (file &optional p)
  "Add new source file to project and touch `prj/cmake-list-file' if
`prj/touch-cmake-lists-at-create-new-file' is t."
  (interactive (list
                (expand-file-name
                 (read-string "File name: ")
                 (ido-read-directory-name "Where to place: "))))
  (let ((p (or p project-details (project-root-fetch))))
    (if p
        (progn
          (find-file file)
          (when prj/touch-cmake-lists-at-create-new-file
            (mapc
             (lambda (cmake-file)
               (call-process "touch" nil 0 nil cmake-file))
             (prj/go-up-dir-find-file
              prj/cmake-list-file p (file-name-directory file)))))
      (message "Project is not found!"))))

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

;;; project tags handling

(defun prj/gen-etags-dir (path pattern tags-file)
  "Generate TAGS-FILE under PATH for given file PATTERN."
  (interactive
   (list
    (ido-read-directory-name "where: ")
    (read-string "pattern: ")
    (read-string "tags file (default \"TAGS\"): " nil nil "TAGS")))
  (let ((default-directory path)
        (pattern (if (string-match "\\`[ \t]*\\'" pattern)
                     ""
                   (format "-name \"%s\"" pattern))))
    (shell-command
     (format "%s %s -type f -regex \"%s\" %s | xargs %s -e -f %s -a"
             (project-root-find-executable) path project-root-file-regexp
             pattern prj/ctags-exec tags-file))))

(defun prj/generate-etags ()
  "Generate TAGS at project root."
  (interactive)
  (let* ((p (or project-details (project-root-fetch)))
         (default-directory (cdr p))
         (tags-file prj/etags-tags-file)
         included-files)
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (message "Generating TAGS file for %s ..." (car p))
    (shell-command
     (format "%s | xargs %s -e -f %s -a"
             (project-root-find-cmd) prj/ctags-exec tags-file))
    (when (setq included-files
                (project-root-data :include-tags p))
      (mapc
       (lambda (file)
         (when (file-exists-p file)
           (shell-command
            (format "%s -e -f %s -a --etags-include=%s"
                    prj/ctags-exec tags-file (expand-file-name file)))))
       included-files))
    (message "Done")))

(defun prj/get-gtags-label (&optional p)
  "Get gtags label."
  (let ((p (or p (project-root-fetch))))
    (or (project-root-data :gtags-label p)
        (getenv "GTAGSLABEL")
        "default")))

(defun prj/get-gtags-conf (&optional p)
  "Get gtags configuration file."
  (let ((p (or p (project-root-fetch))))
    (or (getenv "GTAGSCONF")
        (project-root-data :gtags-conf p)
        prj/gtags-conf-file
        (setq prj/gtags-conf-file
              (ido-read-file-name "Gtags configuration file: ")))))

(defun prj/generate-gtags ()
  "Run gtags at project root."
  (interactive)
  (let* ((p (or project-details (project-root-fetch)))
         (default-directory (cdr p))
         (gtags-label (prj/get-gtags-label p))
         (gtags-conf (prj/get-gtags-conf p)))
    (message "Generating GTAGS for %s ..." (car p))
    (shell-command
     (format "%s | gtags --gtagslabel=%s --gtagsconf=%s --file=-"
             (project-root-find-cmd) gtags-label gtags-conf))
    (message "Done")))

(defun prj/generate-tags ()
  "Generate tags file at project root by ctags or gtags."
  (interactive)
  (if (project-root-data :-use-gtags project-details)
      (prj/generate-gtags)
    (prj/generate-etags)))

(defun prj/update-gtags-single-file (&optional p)
  "Update GTAGS for current project file."
  (let ((fname (buffer-file-name))
        (p (or p (project-root-fetch))))
    (when (and p fname)
      (let ((gtags-label (prj/get-gtags-label))
            (gtags-conf (prj/get-gtags-conf)))
        (call-process prj/global-exec nil nil nil
                      (format "--gtagslabel=%s" gtags-label)
                      (format "--gtagsconf=%s" gtags-conf)
                      (format "--single-update=%s" fname))))))

(defun prj/update-etags-files (tags-file files &optional proc-name)
  "Update TAGS-FILE for FILES. FILES should be a list of files whose tags are
to be updated. PROC-NAME is the name of ctags process. Returns process handle
of ctags."
  (let ((last-time (current-time))
        (proc-name (or proc-name "prj/update-etags-files"))
        p0 p1 elapsed-time)
    (with-temp-buffer
      (insert-file-contents tags-file)
      (mapc
       (lambda (f)
         (goto-char (point-min))
         (while (search-forward (format "\f\n%s," f) nil t)
           (save-excursion
             (if (search-forward "\f" nil t)
                 (setq p1 (line-beginning-position))
               (setq p1 (point-max))))
           (setq p0 (- (line-beginning-position) 2))
           (delete-region p0 p1)))
       files)
      (write-region (point-min) (point-max) tags-file nil 0))
    (setq elapsed-time (float-time (time-since last-time)))
    (eval
     `(start-process
       proc-name nil prj/ctags-exec "-e" "-o" tags-file "-a" ,@files))))

(defun prj/update-etags-single-file (&optional p)
  "Update TAGS for current project file."
  (let* ((fname (buffer-file-name))
         (p (or p project-details (project-root-fetch)))
         (tags-file (project-root-data :-tags-file p)))
    (when (and p fname tags-file)
      (prj/update-etags-files tags-file `(,fname)))))

(defun prj/update-tags-single-file ()
  "Update tags for single file by ctags or gtags."
  (when (and
         (buffer-file-name)
         project-details)
    (when prj/update-tags-verbose
      (message "Updating tags for %s ..." (buffer-file-name)))
    (if (project-root-data :-use-gtags project-details)
        (prj/update-gtags-single-file project-details)
      (prj/update-etags-single-file project-details))
    (when prj/update-tags-verbose
      (message "Done"))))

(defun prj/save-buffers-and-update-tags-sentinel (proc event)
  "Message something after tags file have been updated for a project."
  (when (eq (process-status proc) 'exit)
    (if (zerop (process-exit-status proc))
        (message "Updating tags for %s Done." proc)
      (message "Updating tags for %s failed." proc))))

(defun prj/save-buffers-and-update-tags ()
  "Update tags file for modified buffers of projects asynchronously and save
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
                (setq p project-details)
                (not (string=
                      "none"
                      (project-root-data :tags-tool p))))
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
         (if (project-root-data :-use-gtags p)
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
           (setq proc (prj/update-etags-files
                       (project-root-data :-tags-file p)
                       value
                       key)))
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

(defun prj/set-compile-flags (p buffer)
  "Set compile flags for current BUFFER of project P."
  (let ((packages (project-root-data :use-packages p))
        flags)
    (when packages
      (unless (project-root-data :-compile-flags p)
        (project-root-set-data
         :-compile-flags
         (prj/cmake-find-packages packages)
         p))
      (setq flags (project-root-data :-compile-flags p))
      (with-current-buffer buffer
        (when (derived-mode-p 'f90-mode 'fortran-mode)
          (mapc
           (lambda (path)
             (add-to-list 'flycheck-fortran+-include-paths path))
           (append (plist-get flags :mpi-fortran-include-path)
                   (plist-get flags :hdf5-include-dirs))))))))

(defun prj/cmake-find-packages (packages)
  "Find packages by CMake.

Returns include paths of Fortran, C and CXX."
  (unless (file-directory-p prj/temp-dir)
    (mkdir prj/temp-dir))
  (let ((default-directory prj/temp-dir)
        mpi-fortran-include-path
        mpi-c-include-path
        mpi-cxx-include-path
        hdf5-include-dirs)
    (with-temp-buffer
      (insert prj/cmake-find-packages-headers)
      (when (member 'mpi packages)
        (insert prj/cmake-find-mpi-content))
      (when (member 'hdf5 packages)
        (insert prj/cmake-find-hdf5-content))
      (write-region (point-min) (point-max) prj/cmake-list-file nil 0))
    (with-temp-buffer
      (call-process prj/cmake-exec nil t nil ".")
      (goto-char (point-min))
      (when (re-search-forward
             "\\[prj\\] mpi-fortran-include-path: \\(.+\\)$" nil t)
        (setq mpi-fortran-include-path
              (split-string (match-string-no-properties 1) ";")))
      (goto-char (point-min))
      (when (re-search-forward
             "\\[prj\\] mpi-c-include-path: \\(.+\\)$" nil t)
        (setq mpi-c-include-path
              (split-string (match-string-no-properties 1) ";")))
      (goto-char (point-min))
      (when (re-search-forward
             "\\[prj\\] mpi-cxx-include-path: \\(.+\\)$" nil t)
        (setq mpi-cxx-include-path
              (split-string (match-string-no-properties 1) ";")))
      (goto-char (point-min))
      (when (re-search-forward
             "\\[prj\\] hdf5-include-dirs: \\(.+\\)$" nil t)
        (setq hdf5-include-dirs
              (split-string (match-string-no-properties 1) ";"))))
    `(:mpi-fortran-include-path
      ,mpi-fortran-include-path
      :mpi-c-include-path
      ,mpi-c-include-path
      :mpi-cxx-include-path
      ,mpi-cxx-include-path
      :hdf5-include-dirs
      ,hdf5-include-dirs)))

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
        (p (or project-details (project-root-fetch))))
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
  (let* ((default-directory proot)
         (p (project-root-fetch))
         (included-files (project-root-data :include-tags p)))
    (if (y-or-n-p "Use GNU global? ")
        (shell-command
         (format "%s | gtags --gtagslabel=%s --gtagsconf=%s --file=-"
                 (project-root-find-cmd)
                 (prj/get-gtags-label p)
                 (prj/get-gtags-conf p)))
      (let ((tags-file prj/etags-tags-file))
        (when (file-exists-p tags-file)
          (delete-file tags-file))
        (shell-command
         (format "%s | xargs %s -e -f %s -a"
                 (project-root-find-cmd) prj/ctags-exec tags-file))
        (when included-files
          (mapc
           (lambda (file)
             (when (file-exists-p file)
               (shell-command
                (format "%s -e -f %s -a --etags-include=%s"
                        prj/ctags-exec tags-file (expand-file-name file)))))
           included-files))))))

(defun prj/helm-create-new-file (file)
  (let* ((p (or project-details (project-root-fetch)))
         (default-directory (cdr p)))
    (let ((default-directory
            (ido-read-directory-name "Directory: ")))
      (when (not (file-exists-p default-directory))
        (mkdir default-directory))
      (find-file (read-string "File name: ")))))

(defun prj/helm-files-candidates ()
  (let ((p (or project-details (project-root-fetch))))
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

(defun prj/helm-copy-path-as-kill (candidate)
  (let ((ch (read-char "f=full, d=directory, n=name: ")))
    (mapc
     (lambda (b)
       (cond ((= ch ?f)
              (if (bufferp b)
                  (kill-new (buffer-file-name b))
                (kill-new b)))
             ((= ch ?d)
              (if (bufferp b)
                  (with-current-buffer b
                    (kill-new default-directory))
                (kill-new (file-name-directory b))))
             ((= ch ?n)
              (if (bufferp b)
                  (kill-new (file-name-nondirectory (buffer-file-name b)))
                (kill-new (file-name-nondirectory b)))))
       (message (car kill-ring)))
     (helm-marked-candidates))))

(defun prj/helm-grep-project (proot)
  (let ((default-directory proot))
    (call-interactively 'prj/grep)))

(defun prj/helm-occur-project (proot)
  (let ((default-directory proot))
    (call-interactively 'prj/occur)))

(defun prj/helm-mini ()
  "Pre-configured `helm' to list project buffers, project files and seen projects."
  (interactive)
  (unless (featurep 'helm)
    (require 'helm))
  (helm :sources `(((name . "Project Buffers")
                    (candidates . prj/helm-buffers-candidates)
                    (candidate-number-limit . ,prj/helm-candidate-number-limit)
                    (action . (("Switch to buffer" . switch-to-buffer)
                               ("Switch to buffer other window" . switch-to-buffer-other-window)
                               ("Jump to Dired" . prj/helm-jump-to-dired-buffer)
                               ("Save buffer(s)" . prj/helm-save-buffers)
                               ("Kill buffer(s)" . prj/helm-kill-buffers)
                               ("Multi occur on buffer(s)" . prj/helm-multi-occur)
                               ("Helm multi swoop on buffer(s)" . prj/helm-multi-swoop)
                               ("Copy buffer name(s) as kill" . prj/helm-copy-path-as-kill))))
                   ((name . "Project Files")
                    (candidates . prj/helm-files-candidates)
                    (candidate-number-limit . ,prj/helm-candidate-number-limit)
                    (action . (("Find file(s)" . prj/helm-find-files)
                               ("Find file other window" . find-file-other-window)
                               ("Jump to Dired" . prj/helm-jump-to-dired-file)
                               ("Grep files" . prj/helm-grep-files)
                               ("Copy file name(s) as kill" . prj/helm-copy-path-as-kill))))
                   ((name . "Create New File")
                    (dummy)
                    (action . prj/helm-create-new-file))
                   ((name . "Seen Projects")
                    (candidates . prj/helm-seen-projects)
                    (action . (("Find project root in Dired" . find-file)
                               ("Generate TAGS" . prj/helm-gen-tags)
                               ("Remove from seen projects" . prj/helm-remove-seen-projects)
                               ("Run grep at project files" . prj/helm-grep-project)
                               ("Run multi occur at project buffers" . prj/helm-occur-project)))))
        :buffer "*project helm mini*"))

;;; project helm etags

(defun prj/helm-etags-build-list (tags-file &optional recursively)
  "Get all the tags in TAGS-FILE.

If RECURSIVELY is t, get tags in included tags files recursively; otherwise
only TAGS-FILE will be loaded. Returns a list of tags string with each string
contains tag, file and line number which are split by
`prj/helm-etags-splitter'."
  (let (tags-list file file-end-in-tags tag pm line-num)
    (with-temp-buffer
      (when (file-exists-p tags-file)
        (insert-file-contents tags-file))
      (setq pm (point-max))
      (goto-char (point-min))
      (while (search-forward "\f\n" pm t)
        (setq file (buffer-substring-no-properties
                    (point)
                    (1- (search-forward ","))))
        (setq file-end-in-tags (or
                                (save-excursion
                                  (search-forward "\f" pm t))
                                pm))
        (while (search-forward "\177" file-end-in-tags t)
          (setq tag (buffer-substring-no-properties
                     (line-beginning-position)
                     (1- (point)))
                line-num (buffer-substring-no-properties
                          (search-forward "\001")
                          (1- (search-forward ","))))
          (put-text-property
           0 (length tag) 'face font-lock-variable-name-face tag)
          (setq tags-list
                (cons
                 (format "%s%s%s%s%s"
                         tag prj/helm-etags-splitter file
                         prj/helm-etags-splitter line-num)
                 tags-list))))
      (when recursively
        (goto-char (point-min))
        (while (search-forward ",include\n" pm t)
          (forward-line -1)
          (setq file (buffer-substring-no-properties
                      (point) (1- (search-forward ","))))
          (setq tags-list
                (append (prj/helm-etags-build-list file t)
                        tags-list)))))
    tags-list))

(defun prj/get-tags-file-recursively (tags-file)
  "Get tags file name recursively for given TAGS-FILE."
  (let ((files (list tags-file))
        file)
    (with-temp-buffer
      (when (file-exists-p tags-file)
        (insert-file-contents tags-file))
      (goto-char (point-min))
      (while (search-forward ",include\n" nil t)
        (forward-line -1)
        (setq file (buffer-substring-no-properties
                    (point)
                    (1- (search-forward ","))))
        (add-to-list 'files file)
        (mapc
         (lambda (file)
           (add-to-list 'files file))
         (prj/get-tags-file-recursively file))))
    files))

(defun prj/should-reload-files-p (files p cache-symbol)
  "Determine whether FILES should be reloaded.

Modification file of FILES and CACHE-SYMBOL existence of project P is checked
to determine whether FILES should be reloaded. FILES should be a list of
files. If FILES is nil, the modification time will not be checked."
  (let ((files (or files '(nil)))
        (p (or p project-details (project-root-fetch)))
        yes? mod-time last-mod-time last-mod-time-symbol)
    (catch 'should-reload-p
      (mapc
       (lambda (file)
         (unless (and (if file
                          (file-exists-p file)
                        t)
                      (if file
                          (setq mod-time (nth 5 (file-attributes file)))
                        t)
                      (if file
                          (setq last-mod-time-symbol
                                (intern (format ":-%s-last-mod-time" file)))
                        t)
                      (if file
                          (prog1
                              (setq last-mod-time
                                    (project-root-data last-mod-time-symbol p))
                            (project-root-set-data last-mod-time-symbol
                                                   mod-time p))
                        t)
                      (if file
                          (not (time-less-p last-mod-time mod-time))
                        t)
                      (if cache-symbol
                          (project-root-data cache-symbol p)
                        t))
           (throw 'should-reload-p (setq yes? t))))
       files))
    yes?))

(defun prj/helm-etags-candidates ()
  "Get tags candidates from project tags file.

The tags file is loaded recursively. Tags cache and modification time of the
tags files are checked to determine whether tags should be reloaded."
  (let* ((p (or project-details (project-root-fetch)))
         (tags-file (project-root-data :-tags-file p))
         (tags-files (prj/get-tags-file-recursively tags-file))
         candidates)
    (unless (project-root-data :-etags-hash-cache p)
           (project-root-set-data
            :-etags-hash-cache
            (make-hash-table :test 'equal) p))
    (mapc
     (lambda (tags-file)
       (when (prj/should-reload-files-p (list tags-file) p :-etags-hash-cache)
         (puthash tags-file
                  (prj/helm-etags-build-list tags-file)
                  (project-root-data :-etags-hash-cache p))))
     tags-files)
    (maphash
     (lambda (key val)
       (setq candidates (append val candidates)))
     (project-root-data :-etags-hash-cache p))
    candidates))

(defun prj/helm-etags-goto (switcher c)
  "Go to a tag."
  (let* ((words (split-string c prj/helm-etags-splitter))
         (file (nth 1 words))
         (line-num (string-to-int (nth 2 words))))
    (ring-insert find-tag-marker-ring (point-marker))
    (funcall switcher file)
    (goto-char (point-min))
    (goto-line line-num)
    (recenter)))

(defun prj/helm-etags-match-part (c)
  "Returns part or all of candidate C."
  (if prj/helm-etags-match-part
      (car (split-string c prj/helm-etags-splitter))
    c))

(defun prj/helm-etags-toggle-match-part ()
  "Toggle value of `prj/helm-etags-match-part'."
  (interactive)
  (setq prj/helm-etags-match-part
        (not prj/helm-etags-match-part)))

(defun prj/helm-etags-default ()
  "Default input for `prj/helm-etags'.

Symbol at point will be the default input."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (b0 (car bounds))
         (b1 (cdr bounds))
         (str ""))
    (when (and b0 b1 prj/helm-etags-input-at-point)
      (setq str (format "\\_<%s\\_>" (buffer-substring-no-properties b0 b1))))
    str))

(defun prj/helm-etags ()
  "Preconfigured `helm' for etags."
  (interactive)
  (unless (featurep 'helm)
    (require 'helm))
  (let ((default-input (prj/helm-etags-default))
        (prj/helm-etags-match-part t)
        (helm-execute-action-at-once-if-one
         prj/helm-etags-execute-action-at-once-if-one))
    (helm :sources `(((name . "Project tags")
                      (candidates . prj/helm-etags-candidates)
                      (match-part . prj/helm-etags-match-part)
                      (action . (("Go to tag" . (lambda (c)
                                                  (prj/helm-etags-goto
                                                   'find-file c)))
                                 ("Go to tag other window"
                                  . (lambda (c)
                                      (prj/helm-etags-goto
                                       'find-file-other-window c)))))))
          :input default-input
          :keymap prj/helm-etags-map)))

;;; auto complete ac-sources and company backends

(defun prj/ac-gtags-candidate ()
  "Get auto complete candidates by GNU global. Returns a list of tag string."
  (ignore-errors
    (with-temp-buffer
      (when (eq (call-process prj/global-exec nil t nil "-ci" ac-prefix) 0)
        (goto-char (point-min))
        (let (candidates)
          (while (and (not (eobp))
                      (push
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))
                       candidates)
                      (eq (forward-line) 0)))
          (nreverse candidates))))))

(defun prj/etags-get-tags-candidates (tags-file &optional prefix recursively)
  "Get all tags candidates matching PREFIX from TAGS-FILE.

Returns a list of tag string."
  (let ((prefix-search (if prefix (concat "\177" prefix) "\177"))
        tags buf b0 b1 pm last-time elapsed-time)
    (setq last-time (current-time))
    (with-temp-buffer
      (when (file-exists-p tags-file)
        (insert-file-contents tags-file))
      (setq pm (point-max))
      (goto-char (point-min))
      (while (search-forward prefix-search pm t)
        (setq b0 (1+ (search-backward "\177")))
        (setq b1 (search-forward "\001" (line-end-position) t))
        (when (and b1 (> (setq b1 (1- b1)) b0))
          (setq tags (cons (buffer-substring-no-properties b0 b1) tags))))
      (when recursively
        (goto-char (point-min))
        (while (search-forward ",include\n" pm t)
          (forward-line -1)
          (setq tags (append (prj/etags-get-tags-candidates
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (1- (search-forward ",")))
                              prefix t)
                             tags)))))
    (setq elapsed-time (float-time (time-since last-time)))
    tags))

(defun prj/ac-etags-candidates ()
  "Get etags candidates from tags file of current project."
  (let* ((p (or project-details (project-root-fetch)))
         (tags-file (when p (project-root-data :-tags-file p)))
         (last-ac-cache (when p (project-root-data :-last-ac-cache p)))
         (last-ac-prefix (when p (project-root-data :-last-ac-prefix p)))
         (last-ac-prefix-len (when last-ac-prefix (length last-ac-prefix)))
         (ac-prefix-len (when ac-prefix (length ac-prefix)))
         last-time elapsed-time)
    (setq last-time (current-time))
    (if (and
         last-ac-prefix
         ac-prefix
         (>= ac-prefix-len last-ac-prefix-len)
         (string= (substring ac-prefix 0 last-ac-prefix-len) last-ac-prefix)
         (not (prj/should-reload-files-p (list tags-file) p :-last-ac-cache)))
        (all-completions ac-prefix last-ac-cache)
      (setq last-ac-cache (prj/etags-get-tags-candidates tags-file ac-prefix t))
      (project-root-set-data :-last-ac-prefix ac-prefix p)
      (project-root-set-data :-last-ac-cache last-ac-cache p)
      last-ac-cache)))

(defun prj/ac-define-etags-source ()
  "Define `auto-complete' source for etags."
  (ac-define-source prj/etags
    '((candidates . prj/ac-etags-candidates)
      (candidate-face . prj/ac-etags-candidate-face)
      (selection-face . prj/ac-etags-selection-face)
      (requires . 3))))

(defun prj/ac-define-gtags-source ()
  "Define `auto-complete' source for gtags."
  (ac-define-source prj/gtags
    '((candidates . prj/ac-gtags-candidate)
      (candidate-face . prj/ac-gtags-candidate-face)
      (selection-face . prj/ac-gtags-selection-face)
      (requires . 3))))

(defun prj/ac-setup (p)
  "Setup `auto-complete' for project P."
  (when (prj/use-completion p (current-buffer))
    (unless (featurep 'auto-complete)
      (require 'auto-complete))
    (if (project-root-data :-use-gtags p)
        (progn
          (unless prj/ac-gtags-source-defined
            (prj/ac-define-gtags-source)
            (setq prj/ac-gtags-source-defined t))
          (add-to-list 'ac-sources 'ac-source-prj/gtags))
      (unless prj/ac-etags-source-defined
        (prj/ac-define-etags-source)
        (setq prj/ac-etags-source-defined t))
      (add-to-list 'ac-sources 'ac-source-prj/etags))
    (when (and
           (member major-mode ac-modes)
           (not auto-complete-mode))
      (auto-complete-mode 1))))

(defun prj/use-completion (p buf)
  "Determine whether the BUF (buffer) of P (project) use `company' or
`auto-complete'."
  (let ((yes? prj/use-completion)
        (prj/local-use-comp (project-root-data :use-completion p)))
    (cond ((numberp prj/local-use-comp)
           (if (> prj/local-use-comp 0)
               (setq yes? t)
             (setq yes? nil)))
          ((listp prj/local-use-comp)
           (setq yes? nil)
           (with-current-buffer buf
             (catch 'buf-match
               (mapc
                (lambda (elem)
                  (cond ((symbolp elem)
                         (when (eq major-mode elem)
                           (throw 'buf-match (setq yes? t))))
                        ((stringp elem)
                         (when (eq 0
                                   (string-match
                                    (regexp-quote (expand-file-name elem (cdr p)))
                                    default-directory))
                           (throw 'buf-match (setq yes? t))))))
                prj/local-use-comp)))))
    yes?))

(defun prj/company-etags-candidates (prefix)
  "Get candidates from all tags file recursively matching PREFIX.

Don't worry about the time. I've tested with a TAGS file over 4 MB with the
time elapsed to be 0.03 s. The TAGS file is generated at /usr/include/."
  (let* ((p project-details)
         (tags-file (project-root-data :-tags-file p))
         (last-company-prefix
          (or (project-root-data :-last-company-prefix p) ""))
         (last-company-prefix-len (length last-company-prefix))
         (company-prefix-len (length prefix))
         last-company-cache)
    (if (and
         (>= company-prefix-len last-company-prefix-len)
         (eq 0 (string-match last-company-prefix prefix))
         (not
          (prj/should-reload-files-p
           (list tags-file) p :-last-company-prefix)))
        (all-completions prefix (project-root-data :-last-company-cache p))
      (setq last-company-cache
            (prj/etags-get-tags-candidates tags-file prefix t))
      (project-root-set-data
       :-last-company-cache last-company-cache p)
      (project-root-set-data :-last-company-prefix prefix)
      last-company-cache)))

(defun prj/company-etags-prefix-p ()
  "Get `company' prefix."
  (and
   project-details
   (not (company-in-string-or-comment))
   prj/buffer-is-using-etags-backend
   (or (company-grab-symbol) 'stop)))

(defun prj/company-etags (command &optional arg &rest ignored)
  "`company-mode' completion backend for etags."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'prj/company-etags))
    (prefix (prj/company-etags-prefix-p))
    (candidates (prj/company-etags-candidates arg))))

(defun prj/company-setup (p)
  "Setup `company-mode' for current project buffer."
  (unless (featurep 'company)
    (require 'company))
  (when (prj/use-completion p (current-buffer))
    (unless company-mode
      (company-mode 1))
    (if (project-root-data :-use-gtags)
        (add-to-list 'company-backends
                     `(company-gtags
                       ,@prj/extra-company-backeds))
      (add-to-list 'company-backends
                   `(prj/company-etags
                     ,@prj/extra-company-backeds))
      (setq prj/buffer-is-using-etags-backend t))))

;;; project minor/global mode

(defun prj/load-project-locals (local-file buffer)
  "Load project local variables from `prj/project-locals-file'.

The format of `prj/project-locals-file' is identical to that of
.dir-locals.el."
  (let (locals mode sym val)
    (with-current-buffer buffer
      (when (file-exists-p local-file)
        (with-temp-buffer
          (insert-file-contents local-file)
          (setq locals (read (buffer-string))))
        (mapc
         (lambda (local)
           (setq mode (car local))
           (when (or
                  (and
                   (stringp mode)
                   (eq 0
                       (string-match
                        (regexp-quote
                         (expand-file-name mode (cdr project-details)))
                        default-directory)))
                  (and
                   (symbolp mode)
                   (eq major-mode mode))
                  (and
                   (symbolp mode)
                   (null mode)))
             (mapc
              (lambda (pair)
                (setq sym (car pair)
                      val (cdr pair))
                (when (symbolp sym)
                  (if (eq sym 'eval)
                      (eval val)
                    (eval `(setq-local ,sym val)))))
              (cdr local))))
         locals)))))

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  :lighter prj/buffer-mode-lighter
  (if project-minor-mode
      (progn
        (let ((p (or project-details (project-root-fetch)))
              (fname (buffer-file-name))
              lght
              tags-tool)
          (when (and
                 p
                 fname
                 (not (file-remote-p fname)))
            (let ((default-directory (cdr p)))
              ;; mode line lighter
              (setq
               prj/buffer-mode-lighter
               (if (setq lght (project-root-data :lighter p))
                   (format " Prj:%s" lght)
                 " Prj"))
              ;; touch `prj/cmake-list-file' if needed
              (when (and
                     prj/touch-cmake-lists-at-create-new-file
                     (eval `(derived-mode-p ,@prj/cmake-modes))
                     (= (point-min) (point-max)))
                (mapc
                 (lambda (cmake-file)
                   (call-process "touch" nil 0 nil cmake-file))
                 (prj/go-up-dir-find-file prj/cmake-list-file p
                                          (file-name-directory fname))))
              ;; add update tags hook, determine tags tool, generate tags,
              ;; setup completion
              (unless (or
                       (not prj/use-tags)
                       (string=
                        "none"
                        (setq tags-tool (project-root-data :tags-tool p)))
                       (not prj/tags-tool-found))
                (when (project-root-file-is-project-file fname p)
                  (add-hook 'after-save-hook
                            'prj/update-tags-single-file nil t))
                (cond ((string= "gtags" tags-tool)
                       (project-root-set-data :-use-gtags t p))
                      ((string= "ctags" tags-tool)
                       (project-root-set-data :-use-gtags nil p))
                      (t
                       (project-root-set-data :-use-gtags nil p)))
                (if (project-root-data :-use-gtags p)
                    (unless (file-exists-p "GTAGS")
                      (prj/generate-gtags))
                  (unless (file-exists-p prj/etags-tags-file)
                    (prj/generate-etags))
                  (project-root-set-data
                   :-tags-file
                   (expand-file-name prj/etags-tags-file)))
                (cl-case prj/completion-backend
                  (company (prj/company-setup p))
                  (auto-complete (prj/ac-setup p))))
              ;; auto insert file header
              (when prj/auto-insert-after-find-file
                (auto-insert))
              ;; run project hooks
              (run-hooks (project-root-data :prj-setup-hooks p)))
            ;; load project local variables
            (when (and
                   (file-exists-p
                    (expand-file-name
                     prj/project-locals-file
                     (cdr project-details)))
                   prj/load-project-locals-if-exists)
              (prj/load-project-locals
               (expand-file-name
                prj/project-locals-file
                (cdr project-details))
               (current-buffer)))
            ;; set compile flags
            (when prj/auto-set-flags-if-needed
              (prj/set-compile-flags p (current-buffer))))))
    (remove-hook 'after-save-hook 'prj/update-tags-single-file t)))

(defun project-mode-on-safe ()
  "Enable `global-project-mode' if it is safe to do so.

Enable `global-project-mode' only when all following conditions are meet:
+ buffer has a file name;
+ file exists;
+ file is not a remote file;
+ a project is found. "
  (let ((fname (buffer-file-name)))
    (when (and
           fname
           (not (file-remote-p fname))
           (project-root-fetch))
      (project-minor-mode))))

(define-globalized-minor-mode global-project-mode project-minor-mode
  project-mode-on-safe
  :init-value nil
  :require 'project)

(defun prj/re-turn-on-project-minor-mode (arg)
  "Try re-turn-on `project-minor-mode'. With prefix ARG toggle off and then on
`project-minor-mode' for all buffers, otherwise for current buffer. This is
very useful when you have changed some settings of `project-roots'"
  (interactive "P")
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when project-minor-mode
         (project-minor-mode -1))
       (project-minor-mode)))
   (if arg
       (buffer-list)
     `(,(current-buffer)))))

(provide 'project)

;;; project.el ends here
