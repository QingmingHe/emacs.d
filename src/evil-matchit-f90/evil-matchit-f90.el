;;; evil-matchit-f90.el ---Fortran (f90) plugin of evil-matchit

;; Copyright (C) 2015 Qingming He <906459647@qq.com>

;; Author: Qingming He <906459647@qq.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK don make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

;; ruby/bash/lua/vimrc
(defvar evilmi-f90-match-tags
  '(
    ("function" ()  "endfunction")
    ("subroutine" ()  "endsubroutine")
    ("program" ()  "endprogram")
    ("module" ("contains")  "endmodule")
    ("interface" ()  "endinterface")
    ("if" ("else" "elseif")  ("endif"))
    ("select" ("case" "casedefault")  ("endselect"))
    ("do" () ("enddo"))
    )
  "The table we look up match tags. This is a three column table.
The first column contains the open tag(s).
The second column contains the middle tag(s).
The third column contains the closed tags(s).
The forth column is optional, t means the tags could be function exit
"
  )

;;;###autoload
(defun evilmi-f90-get-tag ()
  (evilmi-sdk-get-tag evilmi-f90-match-tags evilmi-sdk-extract-keyword-howtos)
  )

;;;###autoload
(defun evilmi-f90-jump (rlt NUM)
  (evilmi-sdk-jump rlt NUM evilmi-f90-match-tags evilmi-sdk-extract-keyword-howtos)
  )

(provide 'evil-matchit-f90)
