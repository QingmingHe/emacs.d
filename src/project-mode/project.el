;;; project.el --- minor mode for handling projects
;;
;; Minor mode to update TAGS when a file is saved

;; Copyright (C) 2009  Matt Keller <mattkeller at gmail dot com>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'project-root)
(require 'etags-update)
(require 'flycheck)
(require 'helm)

(defvar project-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "SomeKey") 'some-function)
    nil
    map))

(defvar prj/update-tags t
  "Whether update tags file of the project after saving a file of the project")

(defvar prj/use-gtags (if (executable-find "gtags")
                          t
                        nil)
  "Use gtags or ctags?")

(defvar prj/gtags-conf-file-choices
  '("/usr/share/gtags/gtags.conf"
    "/usr/local/share/gtags/gtags.conf"
    "/opt/share/gtags/gtags.conf"
    "/opt/gtags/share/gtags.conf")
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
   '(lambda (file)
      (when (file-exists-p file)
        (setq prj/gtags-conf-file-dir (file-name-directory file))
        (setq prj/gtags-conf-file-name (file-name-nondirectory file))
        (throw 'gtags-conf-file file)))
   prj/gtags-conf-file-choices))

(defun prj/find-file ()
  "Find a file from a list of those that exist in the current
project. Use completing-read instead of ido-complete-read to make use of helm."
  (interactive)
  (with-project-root
      (let* ((project-files (project-root-files))
             (file (helm-comp-read "Find file in project: "
                                   (mapcar 'car project-files))))
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
  (let ((pname
         (if p
             (format "%s: %s" (car p) (cdr p))
           (helm-comp-read 
            "Enter name of project: "
            (mapcar
             (lambda (p)
               (format "%s: %s" (car p) (cdr p)))
             (or project-root-seen-projects '("None" . "No project root")))))))
    (unless (string= "None: No project root" pname)
      (find-file
       (progn
         (string-match ".+: \\(.+\\)" pname)
         (match-string 1 pname))))))

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
         (gtags-conf (prj/get-gtags-conf))
         (gtags-file "gtags.file"))
    (shell-command
     (format "%s > %s" (project-root-find-cmd) gtags-file))
    (shell-command
     (format "gtags --gtagslabel=%s --gtagsconf=%s -f %s"
             gtags-label gtags-conf gtags-file))
    (delete-file gtags-file)))

(defun prj/generate-tags ()
  (interactive)
  (if prj/use-gtags
      (prj/generate-gtags)
    (prj/generate-gtags)))

(defun prj/update-gtags-single-file ()
  "Update GTAGS for single file only when current buffer is a project file."
  (let ((fname (buffer-file-name))
        (p (project-root-fetch)))
    (when (and
           p
           (project-root-file-is-project-file fname p))
      (message "Updating GTAGS ...")
      (let ((gtags-label (prj/get-gtags-label))
            (gtags-conf (prj/get-gtags-conf)))
        (shell-command
         (format
          "global --gtagslabel=%s --gtagsconf=%s --single-update %s"
          gtags-label gtags-conf fname)))
      (message "Done"))))

(defun prj/update-tags-single-file ()
  (if prj/use-gtags
      (prj/update-gtags-single-file)
    (etu/update-tags-for-file)))

(defun prj/set-gfortran-compile-args (&optional p)
  "Set flycheck-gfortran-* variables."
  (when (string= "f90-mode" (format "%s" major-mode))
      (let ((p (or p (project-root-fetch))))
        (setq flycheck-gfortran-include-path
              (mapcar
               (lambda (include-path)
                 (if (eq 0 (string-match-p "[/~]" include-path))
                     (expand-file-name include-path)
                   (expand-file-name (concat (cdr p) include-path))))
               (or (project-root-data :gfortran-include-paths p) '("."))))
        (when (project-root-data :gfortran-definitions p)
          (setq flycheck-gfortran-definitions
                (project-root-data :gfortran-definitions p)))
        (when (project-root-data :gfortran-language-standard p)
          (setq flycheck-gfortran-language-standard
                (project-root-data :gfortran-language-standard p))))))

(defun prj/set-compile-args (&optional p)
  "Set flycheck-checker-* variables."
  (let ((p (or p (project-root-fetch))))
    (when p
      (if (string= "f90-mode" (format "%s" major-mode))
          (prj/set-gfortran-compile-args p)
        (if (string= "cc-mode" (format "%s" major-mode))
            ())))))

(add-hook 'after-init-hook 'project-root-load-roots)

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  nil
  :lighter " prj"
  (if project-minor-mode
      (progn
        (let ((p (project-root-fetch)))
          (when (and prj/update-tags p)
            (add-hook 'after-save-hook 'prj/update-tags-single-file))
          (prj/set-compile-args p)))
    (progn
      (remove-hook 'after-save-hook 'prj/update-tags-single-file))))

(provide 'project)

;;; project.el ends here
