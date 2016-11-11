((c-mode
  . ((eval
      . (progn
          (when (string-match-p "\\.h$" buffer-file-name)
            (call-interactively 'c++-mode))))))
 (nil
  . ((eval
      . (progn
          (if (executable-find "ccache")
              (setq-local compile-command
                          "python setup.py install --user --fp=double --with-ccache")
            (setq-local compile-command "python setup.py install --user --fp=double"))
          (setq-local compile-dir-default (cdr project-details))))))
 (c++-mode
  . ((eval
      . (progn
          (setq-local flycheck-gcc-openmp t)
          (prj/set-language-flags
           `(,(getenv "_PYTHON_INC_PATH")
             ,@(prj/c++-system-include-paths))
           `("SWIG""FP_PRECISION=double" "VEC_LENGTH=8" "OPENMP" "DOUBLE"
             "VEC_ALIGNMENT=16" "GCC")))))))
