(("src/"
  . ((eval
      . (progn
          (setq whitespace-cleanup-mode-only-if-initially-clean nil)
          (when (derived-mode-p 'fortran-mode 'f90-mode)
            (with-project-root
                (add-to-list 'flycheck-fortran+-include-paths
                             (expand-file-name "src/utilities/"))
                (add-to-list 'flycheck-fortran+-definitions "DBL_REAL")
              (add-to-list
               'flycheck-fortran+-definitions
               "necl_ce_default=\"''\"")
              (add-to-list
               'flycheck-fortran+-definitions
               "necl_mg_default=\"''\"")
              (add-to-list
               'flycheck-fortran+-definitions
               "depletion_lib_default1=\"''\"")
              (add-to-list
               'flycheck-fortran+-definitions
               "depletion_lib_default2=\"''\"")
              (add-to-list
               'flycheck-fortran+-definitions
               "depletion_lib_default3=\"''\"")
              (add-to-list
               'flycheck-fortran+-definitions
               "depletion_lib_default4=\"''\"")
              (setq-local flycheck-fortran+-module-path
                    (if (string-match-p "Drivers" buffer-file-name)
                        (expand-file-name "../build/src/Drivers")
                      (expand-file-name "../build/src")))
              (setq flycheck-fortran+-enable-openmp t))
            (unless flycheck-mode
              (flycheck-mode 1))
            (flycheck-buffer))))))
 (rst-mode
  . ((eval
      . (progn
          (setq whitespace-cleanup-mode-only-if-initially-clean nil))))))
