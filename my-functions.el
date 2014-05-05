(defun my-no-word (fname)
  "Open MS word using antiword."
  (interactive (list (read-file-name "Open MS word file: ")))
  (let (
        (tmp-file-name (format (concat "/tmp/" "." (file-name-nondirectory fname))))
        )
    (progn
      (shell-command
       (format "antiword -m UTF-8 %s > %s" fname tmp-file-name))
      (find-file tmp-file-name)
      )
    )
  )

(defun my-shell-command-to-string (command)
  "Return the STDOUT of shell command without the line break."
  (substring (shell-command-to-string command) 0 -1))

(defun os-path-basename (path)
  "Return the basename of path. Just like os.path.basename in Python"
  (car (last (split-string path "/"))))

(defun my-join-string-list (string-list &optional separator)
  "Join list of string with a separator. (default is space)"
  (mapconcat 'identity string-list
             (if separator
                 separator
               " ")))

(defun my-cygwin-to-win-path (cygwin-path)
  "Transforms Cygwin path to Windows path"
  (concat (file-name-as-directory cygwin-root) (expand-file-name cygwin-path))
  )
