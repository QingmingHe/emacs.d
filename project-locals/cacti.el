((f90-mode
  . ((eval
      . (progn
          (setq-local
           flycheck-fortran+-module-path
           (expand-file-name "../build_syntax/include/"
                             (cdr project-details)))
          (setq-local compile-dir-default
                      (expand-file-name ".." (cdr project-details)))
          (setq-local f90-do-indent 4)
          (setq-local f90-if-indent 4)
          (setq-local f90-type-indent 4)
          (setq-local f90-program-indent 0)
          (setq-local f90-associate-indent 0))))))
