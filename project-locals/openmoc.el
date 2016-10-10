((c-mode
  . ((eval
      . (progn
          (when (string-match-p "\\.h$" buffer-file-name)
            (call-interactively 'c++-mode))))))
 (c++-mode
  . ((eval
      . (progn
          (setq-local flycheck-gcc-openmp t)
          (prj/set-language-flags
           `(,(getenv "_PYTHON_INC_PATH")
             ,@(prj/c++-system-include-paths))
           `("SWIG" "__cplusplus" "FP_PRECISION=double" "VEC_LENGTH=8"
             "OPENMP")))))))
