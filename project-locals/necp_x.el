(("src/" . ((whilecpace-cleanup-mode-only-if-initially-clean . nil)))
 (f90-mode
  . ((eval
      . (progn
          (with-project-root
              (setq-local compile-dir-default (expand-file-name ".."))
              (add-to-list 'flycheck-fortran+-include-paths
                           (expand-file-name "src/utilities/"))
            (add-to-list 'flycheck-fortran+-definitions "DBL_REAL")
            (setq flycheck-fortran+-module-path
                  (expand-file-name "../build_syntax/src")))
          (unless flycheck-mode
            (flycheck-mode 1))
          (flycheck-buffer))))))
