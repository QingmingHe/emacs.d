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

(defvar project-update-etags t)

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

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  nil
  :lighter " prj"
  :after-hook (progn
                (prj/set-compile-args))
  (if project-minor-mode
      (progn
        (when project-update-etags
          (add-hook 'after-save-hook 'etu/update-tags-for-file)))
    (progn
      (remove-hook 'after-save-hook 'etu/update-tags-for-file))))

(provide 'project)
