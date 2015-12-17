((c-mode
  . ((c-basic-offset . 4)
     (tab-width . 4)
     (indent-tabs-mode . t)
     (eval
      . (progn
          (prj/set-language-flags
           `(,(expand-file-name
               "main" (cdr project-details))
             ,(expand-file-name
               "parsers" (cdr project-details))
             ,(cdr project-details))
           `("HAVE_CONFIG_H")))))))
