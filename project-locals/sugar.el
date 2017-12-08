(("src/" . ((whitespace-cleanup-mode-only-if-initially-clean . nil)))
 (f90-mode
  . ((eval
      . (progn
          (with-project-root
              (setq-local compile-dir-default (expand-file-name ".."))
              (setq flycheck-fortran+-module-path
                    (expand-file-name "../build_syntax/include")))
          (unless flycheck-mode
            (flycheck-mode 1))
          (flycheck-buffer))))))
