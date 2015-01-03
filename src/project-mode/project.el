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

(define-minor-mode project-minor-mode
  "Minor mode for handling project."
  nil
  :lighter " prj"
  :after-hook (progn
                (project-root-set-gfortran-include-paths))
  (if project-minor-mode
      (progn
        (add-hook 'after-save-hook 'etu/update-tags-for-file))
    (progn
      (remove-hook 'after-save-hook 'etu/update-tags-for-file))))

(provide 'project)
