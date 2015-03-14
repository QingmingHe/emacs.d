;;; auto-complete-etags.el --- ac source etags
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

(require 'auto-complete)
(require 'etags)
(require 'project)

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defun ac-etags-candidates ()
  (let* ((p (or prj/buffer-project (project-root-fetch)))
         (tags-file (when p (project-root-data :tags-file p)))
         (tags-mod-time (when tags-file (nth 5 (file-attributes tags-file)))))
    (when (and
           p
           tags-file
           (or
            (null (project-root-data :tags-completion-table p))
            (null (project-root-data :tags-mod-time p))
            (time-less-p
             (project-root-data :tags-mod-time p)
             tags-mod-time)))
      (project-root-set-data :tags-mod-time tags-mod-time p)
      (let ((tags-completion-table nil))
        (project-root-set-data
         :tags-completion-table
         (all-completions ac-target (tags-completion-table))
         p)
        (with-current-buffer (get-file-buffer tags-file)
          (kill-buffer))))
    (when (and p tags-file)
      (project-root-data :tags-completion-table p))))

(ac-define-source etags
  '((candidates . ac-etags-candidates)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3)))

(provide 'auto-complete-etags)
