;;; flycheck-fortran-gfortran-zh.el --- A plugin for flycheck.el to check
;;; Fortran with gfortran. Error messages in Chinese are recognized.

;; Copyright (c) 2015 Qingming He <906459647@qq.com>
;; Copyright (C) 2015 Free Software Foundation, Inc.
;;
;; Author:Qingming He <906459647@qq.com>
;; Keywords: convenience languages tools
;; Version: 0.1
;; Package-Requires: ((flycheck "0.22"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A flycheck plugin to recognize gfortran error messages in Chiniese. To use
;; it, add to your init.el:
;; (require 'flycheck-fortran-gfortran-zh)
;; (add-hook 'f90-mode-hook #'(lambda ()
;;           (flycheck-mode)
;;           (flycheck-select-checker 'fortran-gfortran-zh)))
;;
;; Or if you don't want GCC report errors and warnings in Chinese, set LANG
;; environment variable to be "en_US".

;;; Code:

(require 'flycheck)

(defvar flycheck-make-gfortran-options-buffer-local nil
  "Whether make flycheck gfortran options buffer local.")

(flycheck-def-option-var flycheck-gfortran-definitions nil fortran-gfortran
  "A list of definition for GCC Fortran.

The value of this variable is a list of strings, each of which predefines
name as a macro. Default nil."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-gfortran-J-path nil fortran-gfortran
  "Path to generate modules files.

The value of this variable is a string which specifies where to generate
module files. This path will also be added to include path by gfortran by
default. Default nil."
  :type 'string
  :safe #'stringp)

(flycheck-define-checker fortran-gfortran-zh
  "An Fortran syntax checker using GCC. Support Chinese error and warning.

Uses GCC's Fortran compiler gfortran."
  :command ("gfortran"
            "-cpp"
            "-fsyntax-only"
            "-fshow-column"
            "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            ;; Fortran has similar include processing as C/C++
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gfortran-language-standard concat)
            (option "-f" flycheck-gfortran-layout concat
                    flycheck-option-gfortran-layout)
            (option "-J" flycheck-gfortran-J-path concat)
            (option-list "-W" flycheck-gfortran-warnings concat)
            (option-list "-I" flycheck-gfortran-include-path concat)
            (option-list "-D" flycheck-gfortran-definitions concat)
            (eval flycheck-gfortran-args)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line "." column ":\n"
          (= 3 (zero-or-more not-newline) "\n")
          (or "Error" "Fatal Error" "错误" "致命错误") (or ": " "： ") (message) line-end)
   (warning line-start (file-name) ":" line "." column ":\n"
            (= 3 (zero-or-more not-newline) "\n")
            (or "Warning: " "警告： ") (message) line-end))
  :modes (fortran-mode f90-mode))

(add-to-list 'flycheck-checkers 'fortran-gfortran-zh)

(defun flycheck-make-gfortran-options-buffer-local ()
  "Make Gfortran options buffer-local."
  (interactive)
  (make-variable-buffer-local 'flycheck-gfortran-args)
  (make-variable-buffer-local 'flycheck-gfortran-include-path)
  (make-variable-buffer-local 'flycheck-gfortran-language-standard)
  (make-variable-buffer-local 'flycheck-gfortran-layout)
  (make-variable-buffer-local 'flycheck-gfortran-warnings)
  (make-variable-buffer-local 'flycheck-gfortran-definitions)
  (make-variable-buffer-local 'flycheck-gfortran-J-path))

(when flycheck-make-gfortran-options-buffer-local
  (flycheck-make-gfortran-options-buffer-local))

(provide 'flycheck-fortran-gfortran-zh)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-fortran-gfortran-zh.el ends here
