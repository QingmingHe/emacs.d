(("src/"
  . ((eval
      . (progn
          (setq whilecpace-cleanup-mode-only-if-initially-clean nil)
          (when (derived-mode-p 'fortran-mode 'f90-mode)
            (with-project-root
                (setq-local compile-dir-default (expand-file-name ".."))
                (add-to-list 'flycheck-fortran+-include-paths
                             (expand-file-name "src/utilities/"))
              (add-to-list 'flycheck-fortran+-definitions "DBL_REAL")
              (add-to-list
               'flycheck-fortran+-definitions
               (format
                "necl_ce_default=\"'%s'\""
                (expand-file-name
                 "../build_syntax/library/necl_ce_endf70")))
              (add-to-list
               'flycheck-fortran+-definitions
               (format
                "necl_mg_default=\"'%s'\""
                (expand-file-name
                 "../build_syntax/library/necl_gm_endf70_wims69e")))
              (setq flycheck-fortran+-module-path
                    (expand-file-name "../build_syntax/src"))
              (setq flycheck-fortran+-enable-openmp t))
            (unless flycheck-mode
              (flycheck-mode 1))
            (flycheck-buffer)))))))
