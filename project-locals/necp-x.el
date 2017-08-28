(("src/"
  . ((eval
      . (progn
          (setq whilecpace-cleanup-mode-only-if-initially-clean nil)
          (when (derived-mode-p 'fortran-mode 'f90-mode)
            (with-project-root
                (add-to-list 'flycheck-fortran+-include-paths
                             (expand-file-name "src/utilities/"))
              (add-to-list 'flycheck-fortran+-definitions "DBL_REAL")
              (add-to-list
               'flycheck-fortran+-definitions
               (format
                "necl_ce_default=\"'%s'\""
                (expand-file-name
                 "library/necl_ce_endf70")))
              (add-to-list
               'flycheck-fortran+-definitions
               (format
                "necl_mg_default=\"'%s'\""
                (expand-file-name
                 "library/necl_gm_endf70_wims69e")))
              (if (getenv "NECP_X_build_syntax")
                  (setq-local
                   compile-dir-default
                   (expand-file-name
                    (concat
                     (file-name-as-directory
                      (getenv "NECP_X_build_syntax")) "..")))
                (setq-local compile-dir-default (expand-file-name "..")))
              (setq flycheck-fortran+-module-path
                    (if (getenv "NECP_X_build_syntax")
                        (concat (file-name-as-directory
                                 (getenv "NECP_X_build_syntax")) "src")
                      (expand-file-name "../build_syntax/src")))
              (setq flycheck-fortran+-enable-openmp t))
            (unless flycheck-mode
              (flycheck-mode 1))
            (flycheck-buffer)))))))