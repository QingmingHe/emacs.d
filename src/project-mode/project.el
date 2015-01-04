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

(defvar project-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "SomeKey") 'some-function)
    nil
    map))

(defvar project-update-etags t)

(defun prj/set-gfortran-include-paths (&optional p)
  "Set gfortran include paths defined by :gfortran-include-paths, or set
 the current path"
  (when (string= "f90-mode" (format "%s" major-mode))
      (let ((p (or p (project-root-fetch))))
        (when (not flycheck-gfortran-include-path)
            (make-local-variable 'flycheck-gfortran-include-path))
        (setq flycheck-gfortran-include-path
              (mapcar
               (lambda (include-path)
                 (if (eq 0 (string-match-p "[/~]" include-path))
                     (expand-file-name include-path)
                   (expand-file-name (concat (cdr p) include-path))))
               (or (project-root-data :gfortran-include-paths p) '(".")))))))

(defun prj/set-include-paths (&optional p)
  "Set include paths depend on major mode"
  (let ((p (or p (project-root-fetch))))
    (when p
      (if (string= "f90-mode" (format "%s" major-mode))
          (prj/set-gfortran-include-paths p)
        (if (string= "cc-mode" (format "%s" major-mode))
            ())))))

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  nil
  :lighter " prj"
  :after-hook (progn
                (prj/set-include-paths))
  (if project-minor-mode
      (progn
        (when project-update-etags
          (add-hook 'after-save-hook 'etu/update-tags-for-file)))
    (progn
      (remove-hook 'after-save-hook 'etu/update-tags-for-file))))

(provide 'project)
