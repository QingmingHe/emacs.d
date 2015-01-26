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
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

(require 'auto-complete)

(defun get-ac-cmake-candidates ()
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
  (when (and
         (stringp doc)
         (not (string-match-p "Argument.+to --help-.+is not" doc)))
    doc))


(defun ac-cmake-documentation (item)
  (let (doc)
    (setq
     doc
     (cond
      ;; expanded variables
      ((string-match "\\(.+_IS_GNU\\)\\(CXX\\|CC\\|G77\\)\\'" item)
       (shell-command-to-string
        (format
         "cmake --help-variable %s"
         (concat (match-string 1 item) "\\<LANG\\>"))))

      ((string-match "CMAKE_\\(Fortran\\|CXX\\|C\\)\\(.+\\)" item)
       (or
        (ac-cmake-is-valid-doc
         (shell-command-to-string
          (concat "cmake --help-variable " item)))
        (ac-cmake-is-valid-doc
         (shell-command-to-string
          (format
           "cmake --help-variable CMAKE_\\<LANG\\>%s"
           (match-string 2 item))))))

      ((string-match
        "CMAKE_USER_MAKE_RULES_OVERRIDE_\\(Fortran\\|CXX\\|C\\)"
        item)
       (shell-command-to-string
        "cmake --help-variable CMAKE_USER_MAKE_RULES_OVERRIDE_\\<LANG\\>"))

      ((string-match
        "CMAKE_\\(DEBUG\\|RELEASE\\|RELWITHDEBINFO\\|MINSIZEREL\\)_POSTFIX"
        item)
       (shell-command-to-string
        "cmake --help-variable CMAKE_\\<CONFIG\\>_POSTFIX"))

      ((string-match
        "CMAKE_EXE_LINKER_FLAGS_\\(DEBUG\\|RELEASE\\|RELWITHDEBINFO\\|MINSIZEREL\\)"
        item)
       (shell-command-to-string
        "cmake --help-variable CMAKE_EXE_LINKER_FLAGS_[CMAKE_BUILD_TYPE]"))

      ;; all other
      (t (or
          (ac-cmake-is-valid-doc
           (shell-command-to-string (concat "cmake --help-command " item)))
          (ac-cmake-is-valid-doc
           (shell-command-to-string (concat "cmake --help-module " item)))
          (ac-cmake-is-valid-doc
           (shell-command-to-string (concat "cmake --help-variable " item)))))))
    (if (ac-cmake-is-valid-doc doc)
        doc
      (format "%s is not a documented CMake command/module/variable!"))))


(setq ac-cmake-candidates (get-ac-cmake-candidates))


(ac-define-source cmake
  '((candidates . ac-cmake-candidates)
    (document . ac-cmake-documentation)))


(provide 'auto-complete-cmake)

;;; auto-complete-cmake.el ends here
