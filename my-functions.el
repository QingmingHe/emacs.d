(defun my-no-word (fname)
  "Open MS word using antiword."
  (interactive (list (read-file-name "Open MS word file: ")))
  (shell-command (format "antiword -m UTF-8 %s > %s" fname (concat fname ".noword")))
  (find-file (concat fname ".noword")))
