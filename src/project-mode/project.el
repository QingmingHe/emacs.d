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

(defvar project-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "SomeKey") 'some-function)
    nil
    map))

(defvar prj/update-tags t
  "Whether update tags file of the project after saving a file of the project")

(defun prj/find-file ()
  "Find a file from a list of those that exist in the current
project. Use completing-read instead of ido-complete-read to make use of helm."
  (interactive)
  (with-project-root
      (let* ((project-files (project-root-files))
             (file (completing-read "Find file in project: "
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
           (completing-read
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

(defun prj/generate-tags (dir tags-file)
  "Make $PROJECT-TAGS at project root."
  (interactive
   (list
    (read-directory-name "Generate tags at what directory: "
                         (cdr (project-root-fetch)))
    (read-string "Name of tags file: "
                 (concat (car (project-root-fetch)) "-TAGS"))))
  (let ((default-directory dir))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (shell-command
     (format "%s | xargs ctags -e -f %s -a"
             (project-root-find-cmd) tags-file))))

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
  :after-hook (progn
                (prj/set-compile-args))
  (if project-minor-mode
      (progn
        (when prj/update-tags
          (add-hook 'after-save-hook 'etu/update-tags-for-file)))
    (progn
      (remove-hook 'after-save-hook 'etu/update-tags-for-file))))

(provide 'project)
