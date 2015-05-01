(defun yas-c-include-system-or-user (yas-text)
  (if (string= yas-text "\"")
      "\""
    ">"))
