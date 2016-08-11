(("src/" . ((whitespace-cleanup-mode-only-if-initially-clean . nil)))
 (f90-mode
  . ((eval
      . (progn
          (with-project-root
              (setq-local compile-dir-default (expand-file-name ".."))
              (setq flycheck-fortran+-module-path
                    (expand-file-name "../BUILD_SYNTAX/include")))
          (unless flycheck-mode
            (flycheck-mode 1))
          (flycheck-buffer))))))
