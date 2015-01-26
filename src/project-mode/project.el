;;; project.el --- minor mode for handling projects
;;
;; Copyright (C) 2015  Qingming He
;;
;; Author: Qingming He <906459647@qq.com>
;; Keywords: completion, convenience
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
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'project-root)
(require 'etags-update)

(defvar prj/use-gtags (if (executable-find "gtags")
                          t
                        nil)
  "Use gtags or ctags?")

(defvar prj/gtags-conf-file-choices
  `("/usr/share/gtags/gtags.conf"
    "/usr/local/share/gtags/gtags.conf"
    "/opt/share/gtags/gtags.conf"
    "/opt/gtags/share/gtags.conf"
    ,(expand-file-name "~/.globalrc")
    "/etc/gtags.conf")
  "Default gtags configuration files.")

(defvar prj/gtags-conf-file-dir nil
  "Directory of gtags configuration file")

(defvar prj/gtags-conf-file-name nil
  "File name of gtags configuration file")

(defvar prj/gtags-label-choices '("default" "ctags" "pygments" "native")
  "Gtags label. If \"default\" gtags uses its built in parser; if \"ctags\"
use ctags parser.")

;; Get the gtags configuration file.
(catch 'gtags-conf-file
  (mapcar
   #'(lambda (file)
       (when (file-exists-p file)
         (setq prj/gtags-conf-file-dir (file-name-directory file))
         (setq prj/gtags-conf-file-name (file-name-nondirectory file))
         (throw 'gtags-conf-file file)))
   prj/gtags-conf-file-choices))

(defun prj/find-file ()
  "Find a file from a list of those that exist in the current project."
  (interactive)
  (with-project-root
      (let* ((project-files (project-root-files))
             (file (if (featurep 'helm)
                       (helm-comp-read "Find file in project: "
                                       (mapcar 'car project-files))
                     (ido-completing-read "Find file in project: "
                                          (mapcar 'car project-files)))))
        (find-file (cdr (assoc file project-files))))))

(defun prj/find-grep (grep-regexp)
  "Run grep with find at project root."
  (interactive (list
                (read-string
                 (format "grep regexp (default %s): " (word-at-point)))))
  (with-project-root
      (find-grep
       (format "%s -exec grep -nH -e %s {} +"
               (project-root-find-cmd)
               (if (string-empty-p grep-regexp)
                   (word-at-point)
                 grep-regexp)))))

(defun prj/goto-project (&optional p)
  "Goto a project root for a given project or a read project name."
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
           "Name of tags file: "
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
       "Gtags label (You'd better set project-roots): "
       prj/gtags-label-choices)))

(defun prj/get-gtags-conf ()
  "Get gtags conf."
  (or (getenv "GTAGSCONF")
      (project-root-data :gtags-conf p)
      (ido-read-file-name
       "Gtags configuration file (You'd better set GTAGSCONF env var): "
       prj/gtags-conf-file-dir
       prj/gtags-conf-file-name)))

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
  (interactive)
  (if prj/use-gtags
      (prj/generate-gtags)
    (prj/generate-gtags)))

(defun prj/update-gtags-single-file ()
  "Update GTAGS for single file only when current buffer is a project file."
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
  (if prj/use-gtags
      (prj/update-gtags-single-file)
    (etu/update-tags-for-file)))

(defun prj/kill-buffers-dir ()
  "Kill all buffers under directory."
  (interactive)
  (let ((dir (ido-read-directory-name "Directory: ")))
    (mapcar
     #'(lambda (buffer)
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
     #'(lambda (buffer)
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
        ;; user gfortran include paths
        (mapc
         (lambda (include-path)
           (add-to-list 'flycheck-gfortran-include-path
                        (if (equal 0 (string-match-p "[/~]" include-path))
                            (expand-file-name include-path)
                          (expand-file-name (concat (cdr p) include-path)))))
         (or (project-root-data :gfortran-include-paths p) '(".")))
        ;; user gfortran pre-definitions
        (when (project-root-data :gfortran-definitions p)
          (setq flycheck-gfortran-definitions
                (project-root-data :gfortran-definitions p)))
        ;; user gfortran language standard
        (when (project-root-data :gfortran-language-standard p)
          (setq flycheck-gfortran-language-standard
                (or (project-root-data :gfortran-language-standard p) "f2008")))
        ;; whether use hdf5 library
        (when (project-root-data :use-hdf5 p)
          (let ((hdf5-root (getenv "HDF5_ROOT")))
            (when hdf5-root
              (add-to-list 'flycheck-gfortran-include-path
                           (concat hdf5-root "/include"))))))))

(defun prj/c-include-paths-pkgs (pkgs)
  "Get c include pahts for `pkgs'. `pkgs' should be string or list of string.

Valid form of `pkgs':
\"glib\", \"glib python-2.7\", '(\"glib\" \"python-2.7\")

Returns:
Include paths of `pkgs', including \"-I\" flag."
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
                      (setq lght " prj")
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
  "Enable `project-mode' if it is safe to do so."
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

(add-hook 'after-init-hook 'project-root-load-roots)
(add-hook 'kill-emacs-hook 'project-root-save-roots)

(provide 'project)

;;; project.el ends here
