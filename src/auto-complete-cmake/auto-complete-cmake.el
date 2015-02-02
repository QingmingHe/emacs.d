;;; auto-complete-cmake.el --- ac-source for cmake
;;
;; Copyright (C) 2015 Qingming He
;;
;; Author: Qingming He <906459647@qq.com>
;; Keywords: completion
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
;;
;;; Code:

(require 'auto-complete)

(defvar ac-cmake-executable "cmake"
  "Executable of CMake.")

(defvar ac-cmake-help-commands
  '("--help-command"
    "--help-module"
    "--help-variable")
  "Help commands of CMake.")

(defvar ac-cmake-help-command-candidates
  (make-hash-table :test 'equal)
  "Hash table that stores (CANDIDATE, COMMAND INDEX) pairs.
\"CANDIDATE\" is CMake command/module/variable candidate and \"COMMAND INDEX\"
is index of help command in `ac-cmake-help-commands' for the \"CANDIDATE\".")

(defun get-ac-cmake-candidates ()
  "Get cmake variable/module/command candidates. Symbol such as <LANG>,
<CONFIG> and [CMAKE_BUILD_TYPE] will be expanded; other such as [Project
name], <PackageName> and <NNNN> will not."
  (let (candidates)
    (mapc
     (lambda (command)
       (mapc
        (lambda (keyword)
          (cond
           ;; CMAKE_COMPILER_IS_GNU<LANG>
           ((string-match "\\(.+_IS_GNU\\)<LANG>" keyword)
            (mapc
             (lambda (language)
               (add-to-list
                'candidates
                (concat (match-string 1 keyword) language)))
             '("CXX" "CC" "G77")))

           ;; CMAKE_<LANG>_.+, CMAKE_USER_MAKE_RULES_OVERRIDE_<LANG>
           ((string-match "<LANG>" keyword)
            (mapc
             (lambda (language)
               (add-to-list
                'candidates
                (replace-match language t nil keyword)))
             '("Fortran" "CXX" "C")))

           ;; CMAKE_<CONFIG>_POSTFIX and
           ;; CMAKE_EXE_LINKER_FLAGS_[CMAKE_BUILD_TYPE]
           ((string-match
             "\\(\\[CMAKE_BUILD_TYPE\\]\\|<CONFIG>\\)"
             keyword)
            (mapc
             (lambda (type)
               (add-to-list
                'candidates
                (replace-match type t nil keyword)))
             '("DEBUG" "RELEASE" "RELWITHDEBINFO" "MINSIZEREL")))

           ;; [Project name]_BINARY_DIR, [Project name]_SOURCE_DIR
           ((string-match "\\[Project name\\]" keyword) nil)

           ;; CMAKE_DISABLE_FIND_PACKAGE_<PackageName>
           ((string-match "<PackageName>" keyword)
            (add-to-list 'candidates keyword))

           ;; CMAKE_POLICY_DEFAULT_CMP<NNNN>
           ((string-match "<NNNN>" keyword)
            (add-to-list 'candidates keyword))

           ;; empty
           ((string= "" keyword) nil)
           (t (add-to-list 'candidates keyword))))
        (cdr (split-string (shell-command-to-string command) "\n"))))
     '("cmake --help-command-list"
       "cmake --help-module-list"
       "cmake --help-variable-list"))
    candidates))

(defun ac-cmake-is-valid-doc (doc)
  "Determines whether a doc is valid CMake help doc."
  (when (and
         (stringp doc)
         (not (string-match-p "Argument.+to --help-.+is not" doc)))
    doc))

(defun ac-cmake-get-doc (symbol &optional index)
  "Get CMake help documentation.

SYMBOL: CMake variable/module/command.
INDEX: Index of CMake help command which is passed to
`ac-cmake-executable'. All of the commands are defined in
`ac-cmake-help-commands'. If INDEX is not given, it will be get from
`ac-cmake-help-command-candidates' or scan all the `ac-cmake-help-commands' and
determine which one is the correct.

If documentation of SYMBOL is found, the doc will be returned as string,
otherwise return nil."
  (let ((index (cond (index `(,index))
                     ((gethash symbol ac-cmake-help-command-candidates)
                      `(,(gethash symbol ac-cmake-help-command-candidates)))
                     (t '(0 1 2))))
        doc)
    (catch 'doc-found
      (mapc
       (lambda (i)
         (with-temp-buffer
           (call-process
            ac-cmake-executable
            nil t nil
            (nth i ac-cmake-help-commands) symbol)
           (setq doc (buffer-string)))
         (if (ac-cmake-is-valid-doc doc)
             (progn
               (puthash symbol i ac-cmake-help-command-candidates)
               (throw 'doc-found t))
           (setq doc nil)))
       index))
    doc))

(defun ac-cmake-documentation (item)
  "Get CMake help documentation for ITEM.

ITEM: expanded CMake variable/module/command.

See also `ac-cmake-get-doc'."
  (let (doc)
    (setq
     doc
     (cond
      ((string-match "\\(.+_IS_GNU\\)\\(CXX\\|CC\\|G77\\)\\'" item)
       (ac-cmake-get-doc (concat (match-string 1 item) "\<LANG\>") 2))

      ((string-match "CMAKE_\\(Fortran\\|CXX\\|C\\)\\(.+\\)" item)
       (or
        (ac-cmake-get-doc item 2)
        (ac-cmake-get-doc
         (format "CMAKE_\<LANG\>%s" (match-string 2 item))
         2)))

      ((string-match
        "CMAKE_USER_MAKE_RULES_OVERRIDE_\\(Fortran\\|CXX\\|C\\)"
        item)
       (ac-cmake-get-doc "CMAKE_USER_MAKE_RULES_OVERRIDE_\<LANG\>" 2))

      ((string-match
        "CMAKE_\\(DEBUG\\|RELEASE\\|RELWITHDEBINFO\\|MINSIZEREL\\)_POSTFIX"
        item)
       (ac-cmake-get-doc
        "CMAKE_\<CONFIG\>_POSTFIX" 2))

      ((string-match
        "CMAKE_EXE_LINKER_FLAGS_\\(DEBUG\\|RELEASE\\|RELWITHDEBINFO\\|MINSIZEREL\\)"
        item)
       (ac-cmake-get-doc
        "CMAKE_EXE_LINKER_FLAGS_[CMAKE_BUILD_TYPE]" 2))

      (t (ac-cmake-get-doc item))))
    (if (ac-cmake-is-valid-doc doc)
        doc
      (format "%s is not a documented CMake command/module/variable!" item))))

(setq ac-cmake-candidates (get-ac-cmake-candidates))

(ac-define-source cmake
  '((candidates . ac-cmake-candidates)
    (document . ac-cmake-documentation)))

(provide 'auto-complete-cmake)

;;; auto-complete-cmake.el ends here
