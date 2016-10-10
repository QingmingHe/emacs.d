((f90-mode
  . ((eval
      . (progn
          (setq whilecpace-cleanup-mode-only-if-initially-clean nil)
          (setq-local compile-dir-default
                      (expand-file-name "~/codes/openmc"))
          (add-to-list 'flycheck-fortran+-definitions "UNIX")
          (setq flycheck-fortran+-module-path
                (expand-file-name "~/codes/openmc/build_syntax/include"))
          (if (getenv "HDF5_ROOT")
              (add-to-list
               'flycheck-fortran+-include-paths
               (expand-file-name "include" (getenv "HDF5_ROOT")))
            (warn "Please Set HDF5_ROOT env var")))))))
